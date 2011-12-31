package net.sf.eclipsefp.haskell.buildwrapper;

import java.util.List;

import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.OccurrencesHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPointHandler;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.text.IDocument;
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
	      //buildJob.setRule( getProject() );
	      buildJob.setPriority(Job.BUILD);
	      realFacade.getBuildJobQueue().addJob(buildJob);
	      //buildJob.schedule();
	}

	public void synchronize(final boolean force) {
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
	      buildJob.setRule( getProject() );
	      buildJob.setPriority(Job.BUILD);
	      buildJob.schedule();
	}
	
	public void outline(final IFile f,final OutlineHandler handler){
		final String jobNamePrefix = NLS.bind(BWText.outline_job_name, getProject().getName());

	      Job buildJob = new Job (jobNamePrefix) {
	        @Override
	        protected IStatus run(IProgressMonitor monitor) {
	          try {
	            monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	            OutlineResult or=realFacade.outline(f);
	            handler.handleOutline(or.getOutlineDefs());
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
	
	public void updateFromEditor(final IFile file,final IDocument doc,final OutlineHandler handler){
		final String jobNamePrefix = NLS.bind(BWText.editor_job_name, getProject().getName());
	
	    Job buildJob = new Job (jobNamePrefix) {
	      @Override
	      protected IStatus run(IProgressMonitor monitor) {
	        try {
	          monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	          //long t0=System.currentTimeMillis();
	          /*if (doc!=null){
	        	  realFacade.write(f, doc.get()); // the write is done by ScionTokenScanner
	          }*/
	          //long t1=System.currentTimeMillis();
	          boolean buildOK= realFacade.build1(file);
	          		
	          //long t2=System.currentTimeMillis();
	          
	          List<OutlineDef> defs=realFacade.outline(file).getOutlineDefs();
	          //long t3=System.currentTimeMillis();
	          if (defs.size()>0 && buildOK){
	        	  handler.handleOutline(defs); // avoid removing all outline on error
	          }
	          
	          //long t4=System.currentTimeMillis();
	          //BuildWrapperPlugin.logInfo("write:"+(t1-t0)+"ms,outline:"+(t3-t2)+"ms,handleroutline:"+(t4-t3)+"ms,build:"+(t2-t1)+"ms");
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
	    buildJob.schedule();
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
	
	
	public void getThingAtPoint(final IFile file,final Location location,
			final boolean qualify, final boolean typed,final ThingAtPointHandler handler){
		final String jobNamePrefix = NLS.bind(BWText.thingatpoint_job_name, getProject().getName());
		
	    Job buildJob = new Job (jobNamePrefix) {
	      @Override
	      protected IStatus run(IProgressMonitor monitor) {
	        try {
	          monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	          handler.handleThing(realFacade.getThingAtPoint(file, location, qualify, typed));
	         
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

}
