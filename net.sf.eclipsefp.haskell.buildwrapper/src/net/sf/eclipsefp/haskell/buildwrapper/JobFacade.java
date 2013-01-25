/**
 *  Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper;

import java.util.Collection;

import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportCleanHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.NameDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.NameDefHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.OccurrencesHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPointHandler;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageThread;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.util.SingleJobQueue;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.osgi.util.NLS;
import org.json.JSONArray;

/**
 * performs operations in a job
 * @author JP Moresmau
 *
 */
public class JobFacade  {
	private BWFacade realFacade;

	
	public JobFacade(BWFacade realF){
		realFacade=realF;
	}
	

	public static class BuildJob extends Job {
		private JSONArray notes;
		private BuildOptions buildOptions;
		private BWFacade realFacade;
		
		public BuildJob(String name,BWFacade realFacade,BuildOptions buildOptions) {
			super(name);
			this.realFacade=realFacade;
			this.buildOptions=buildOptions;
			
		}
		@Override
        protected IStatus run(IProgressMonitor monitor) {
          try {
           notes=realFacade.build(buildOptions);
	       if (buildOptions.isOutput()){
	   			try {
	   				IResource res=realFacade.getProject().findMember(BWFacade.DIST_FOLDER);
	   				if (res!=null){
	   					res.refreshLocal(IResource.DEPTH_INFINITE, monitor);
	   				} else {
	   					realFacade.getProject().refreshLocal(IResource.DEPTH_INFINITE, monitor);
	   				}
	   			} catch (CoreException ce){
	   				BuildWrapperPlugin.logError(BWText.error_refreshLocal, ce);
	   				ce.printStackTrace();
	   			}
	   		}
          } finally {
            monitor.done();
          }
          return Status.OK_STATUS;
        }
		
		public JSONArray getNotes() {
			return notes;
		}
	}
	
	public void build(final BuildOptions buildOptions) {
		final String jobNamePrefix = NLS.bind(BWText.job_build, getProject().getName());
		
	    final BuildJob buildJob=new BuildJob(jobNamePrefix,realFacade,buildOptions);
	    
	     buildJob.addJobChangeListener(new JobChangeAdapter(){
	    	  @Override
	    	public void done(IJobChangeEvent event) {
	    		if (event.getResult().isOK()){
	    			Job parseJob = new Job (jobNamePrefix) {
	    				protected IStatus run(IProgressMonitor arg0) {
	    					realFacade.parseBuildResult(buildJob.getNotes());
	    					return Status.OK_STATUS;
	    				};
	    			};
	    			parseJob.setRule( getProject() );
	    			parseJob.setPriority(Job.BUILD);
	    			parseJob.schedule();
	    		}
	    	}
	      });
	      // not needed since we put the job in a queue ourselves
	      // except if both synchronized and build are run concurrently
	      //buildJob.setRule( getProject() );
	      buildJob.setPriority(Job.BUILD);
	      realFacade.getBuildJobQueue().addJob(buildJob);
	}

	public void synchronizeAndBuild(final boolean force,final BuildOptions buildOptions){
		Job sj=getSynchronizeJob(force);
		sj.addJobChangeListener(new JobChangeAdapter(){
	    	  @Override
	    	public void done(IJobChangeEvent event) {
	    		if (event.getResult().isOK()){
	    			build(buildOptions);
	    		}
	    	  }
		});
		sj.schedule();
	}
	
	/**
	 * a simple rule only conflicting with itself
	 */
	private static final ISchedulingRule synchronizeRule=new ISchedulingRule() {
		
		@Override
		public boolean isConflicting(ISchedulingRule arg0) {
			return this==arg0;
		}
		
		@Override
		public boolean contains(ISchedulingRule arg0) {
			return this==arg0;
		}
	};
	
	private Job getSynchronizeJob(final boolean force) {
		final String jobNamePrefix = NLS.bind(BWText.job_synchronize, getProject().getName());

	      Job buildJob = new Job (jobNamePrefix) {
	        @Override
	        protected IStatus run(IProgressMonitor monitor) {
	          try {
	            monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	            realFacade.synchronize(force);
	          } finally {
	            monitor.done();
	          }
	          return Status.OK_STATUS;
	        }
	        
	      };
	      // why? 
	      // build is a project only rule, but synchronize is hard on the disk, so we only want one synchronize at the same time
	      // but we don't want buildJob.setRule( getProject().getWorkspace().getRoot() ); because then any other operation will wait for synchronize
	      // so we only disallow concurrent synchronize, but don't influence with the rest (other operations)
	      buildJob.setRule(synchronizeRule);
	      buildJob.setPriority(Job.BUILD);
	      buildJob.addJobChangeListener(new JobChangeAdapter(){
	    	  /* (non-Javadoc)
	    	 * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
	    	 */
	    	@Override
	    	public void done(IJobChangeEvent event) {
	    		if (event.getResult().isOK() && BuildWrapperPlugin.getDefault()!=null){
	    			UsageThread ut=BuildWrapperPlugin.getDefault().getUsageThread();
	    			if (ut!=null){
	    				ut.addProject(getProject());
	    			}
	    		}
	    	}
	      });
	      return buildJob;
	}
	
	public void synchronize(final boolean force) {
		realFacade.getSynchronizeJobQueue().addJob(getSynchronizeJob(force));
	}
	
	public void outline(final IFile f,final OutlineHandler handler){
		final String jobNamePrefix = NLS.bind(BWText.outline_job_name, getProject().getName());

	      Job buildJob = new Job (jobNamePrefix) {
	        @Override
	        protected IStatus run(IProgressMonitor monitor) {
	          try {
	            monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	            OutlineResult or=realFacade.outline(f);
	            if(!monitor.isCanceled()){
	            	 handler.handleOutline(or);
	            }
	          } finally {
	            monitor.done();
	          }
	          return Status.OK_STATUS;
	        }
	      };
	      buildJob.setRule( f );
	      buildJob.setPriority(Job.SHORT);
	      buildJob.schedule();
	}
	
	//final IDocument doc,
	/**
	 * update the info the editor needs
	 * @param file
	 * @param handler
	 * @param ndhandler
	 */
	public void updateFromEditor(final IFile file,final OutlineHandler handler,final NameDefHandler ndhandler){
		final String jobNamePrefix = NLS.bind(BWText.editor_job_name, getProject().getName());
	
		/*
		 *  we're not automatically building, and it's the first time we get a request from the editor
		 *  we're going to call a full synchronize which in turn will request a configure
		 *  so that we're ready to build our haskell files
		 */
		final boolean needSynchronize=!realFacade.hasEditorSynchronizeQueue(file) && !ResourcesPlugin.getWorkspace().getDescription().isAutoBuilding();
		
	    Job buildJob = new Job (jobNamePrefix) {
	      @Override
	      protected IStatus run(IProgressMonitor monitor) {
	        try {
	          monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	          long t0=System.currentTimeMillis();
	          if (needSynchronize){
	        	  realFacade.synchronize(false);
	          }
	          long t1=System.currentTimeMillis();
	          OutlineResult or=realFacade.outline(file);
	          long t2=System.currentTimeMillis();
	           handler.handleOutline(or); // avoid removing all outline on error
	         
	          long t3=System.currentTimeMillis();
	          
	          
         	  Collection<NameDef> ns=realFacade.build1LongRunning(file,false);
        	  long t35=System.currentTimeMillis();
	          if (ndhandler!=null){
	        	  ndhandler.handleNameDefs(ns);
	          }
	          if (BWFacade.logBuildTimes){
		    	 long t4=System.currentTimeMillis();
	             BuildWrapperPlugin.logInfo("sync:"+(t1-t0)+",outline:"+(t2-t1)+"ms,handleroutline:"+(t3-t2)+"ms,build:"+(t35-t3)+"ms,handleNameDefs:"+(t4-t35)+"ms");
		      }
	        } finally {
	          monitor.done();
	        }
	        return Status.OK_STATUS;
	      }
	    };
	    String path=file.getProjectRelativePath().toOSString();
	    // schedule using target file to not stop operations on source file (like save...)
	    IResource r=file.getProject().findMember(BWFacade.DIST_FOLDER+"/"+path);
		buildJob.setRule( r );
	    buildJob.setPriority(Job.SHORT);
	    SingleJobQueue sjq=realFacade.getEditorSynchronizeQueue(file);
	    sjq.addJob(buildJob);
	    //buildJob.schedule();
	}
	
	public void getOccurrences(final IFile file,final String token,final OccurrencesHandler handler){
		final String jobNamePrefix = NLS.bind(BWText.occurrences_job_name, getProject().getName());
		
	    Job buildJob = new Job (jobNamePrefix) {
	      @Override
	      protected IStatus run(IProgressMonitor monitor) {
	        try {
	          monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	          handler.handleOccurrences(realFacade.getOccurrences(file, token));
	         
	        } finally {
	          monitor.done();
	        }
	        return Status.OK_STATUS;
	      }
	    };
	    buildJob.setRule( file );
	    buildJob.setPriority(Job.SHORT);
	    buildJob.schedule();
	}
	
	
	public void getThingAtPoint(final IFile file,final Location location,final ThingAtPointHandler handler){
		final String jobNamePrefix = NLS.bind(BWText.thingatpoint_job_name, getProject().getName());
		
	    Job buildJob = new Job (jobNamePrefix) {
	      @Override
	      protected IStatus run(IProgressMonitor monitor) {
	        try {
	          monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	          //long t0=System.currentTimeMillis();
	          handler.handleThing(realFacade.getThingAtPoint(file, location));
	          //long t1=System.currentTimeMillis();
	          //BuildWrapperPlugin.logInfo("thingAtPoint:"+(t1-t0)+"ms");
	        } finally {
	          monitor.done();
	        }
	        return Status.OK_STATUS;
	      }
	    };
	    buildJob.setRule( file );
	    buildJob.setPriority(Job.SHORT);
	    //buildJob.schedule();
	    realFacade.getThingAtPointJobQueue(file).addJob(buildJob);
	}
	
	
	public IProject getProject() {
		return realFacade.getProject();
	}

	public void cleanImport(final IFile file,final boolean format, final ImportCleanHandler handler){
		final String jobNamePrefix = NLS.bind(BWText.job_import_clean, file.getName());
		
	    Job buildJob = new Job (jobNamePrefix) {
	      @Override
	      protected IStatus run(IProgressMonitor monitor) {
	        try {
	          monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	          //long t0=System.currentTimeMillis();
	          handler.handleImportCleans(realFacade.cleanImports(file,format));
	          //long t1=System.currentTimeMillis();
	          //BuildWrapperPlugin.logInfo("thingAtPoint:"+(t1-t0)+"ms");
	        } finally {
	          monitor.done();
	        }
	        return Status.OK_STATUS;
	      }
	    };
	    buildJob.setRule( file );
	    buildJob.setPriority(Job.SHORT);
	    //buildJob.schedule();
	    realFacade.getThingAtPointJobQueue(file).addJob(buildJob);
	}
}
