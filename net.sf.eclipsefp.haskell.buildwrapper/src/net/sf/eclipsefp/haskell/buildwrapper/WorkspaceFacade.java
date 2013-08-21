/**
 *  Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper;

import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.actions.WorkspaceModifyOperation;

/**
 * performs operations in a WorkspaceModifyOperation
 * @author JP Moresmau
 *
 */
public class WorkspaceFacade {
	private BWFacade realFacade;
	private IProgressMonitor monitor;
	
	public WorkspaceFacade(BWFacade realF,IProgressMonitor monitor){
		realFacade=realF;
		this.monitor=monitor;
	}
	
//	public void build(final BuildOptions buildOptions) {
//		/*WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(getProject()){
//	    	@Override
//	    	protected void execute(IProgressMonitor arg0) throws CoreException,
//	    			InvocationTargetException, InterruptedException {
//	    		 realFacade.build(buildOptions);
//	    	}
//	    };
//		try {
//			wmo.run(monitor);
//		} catch (InterruptedException ie){
//			// noop
//		}catch (InvocationTargetException ie){
//			BuildWrapperPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
//		}*/
//
//		JobFacade jf=BuildWrapperPlugin.getJobFacade(getProject());
//		if (jf!=null){
//			final String jobNamePrefix = NLS.bind(BWText.job_build, getProject().getName());
//			
//		    final BuildJob buildJob=new BuildJob(jobNamePrefix,realFacade,buildOptions);
//		    
//		     buildJob.addJobChangeListener(new JobChangeAdapter(){
//		    	  @Override
//		    	public void done(IJobChangeEvent event) {
//		    		if (event.getResult().isOK()){
//		    			WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(getProject()){
//		    				@Override
//		    		    	protected void execute(IProgressMonitor arg0) throws CoreException,
//		    		    			InvocationTargetException, InterruptedException {
//		    					realFacade.parseBuildResult(buildJob.getNotes());
//		    				};
//		    			};
//		    			try {
//		    				wmo.run(monitor);
//		    			} catch (InterruptedException ie){
//		    				// noop
//		    			}catch (InvocationTargetException ie){
//		    				BuildWrapperPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
//		    			}
//		    		}
//		    	}
//		      });
//		      //buildJob.setRule( getProject() );
//		      buildJob.setPriority(Job.BUILD);
//		      realFacade.getBuildJobQueue().addJob(buildJob);
//		}
//	}

	public IProject getProject() {
		return realFacade.getProject();
	}

	public void synchronize(final boolean force) {
		WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(getProject()){
	    	@Override
	    	protected void execute(IProgressMonitor arg0) throws CoreException,
	    			InvocationTargetException, InterruptedException {
	    		 realFacade.synchronize(force);
	    	}
	    };
		try {
			wmo.run(monitor);
		} catch (InterruptedException ie){
			// noop
		}catch (InvocationTargetException ie){
			BuildWrapperPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
		}
	}
	
	public void synchronizeAndBuild(final boolean force,final BuildOptions buildOptions) {
		WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(getProject()){
	    	@Override
	    	protected void execute(IProgressMonitor arg0) throws CoreException,
	    			InvocationTargetException, InterruptedException {
	    		 realFacade.synchronize(force);
	    		 realFacade.build(buildOptions);
	    	}
	    };
		try {
			wmo.run(monitor);
		} catch (InterruptedException ie){
			// noop
		}catch (InvocationTargetException ie){
			BuildWrapperPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
		}
	}

	/**
	 * perform a clean
	 * @param mon
	 * @throws CoreException
	 */
	public void clean(IProgressMonitor mon) throws CoreException{
		WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(getProject()){
	    	@Override
	    	protected void execute(IProgressMonitor arg0) throws CoreException,
	    			InvocationTargetException, InterruptedException {
	    		realFacade.clean(arg0);
	    	}
	    };
		try {
			wmo.run(monitor);
		} catch (InterruptedException ie){
			// noop
		}catch (InvocationTargetException ie){
			BuildWrapperPlugin.logError(BWText.error_clean, ie.getCause());
		}
		
	}
}
