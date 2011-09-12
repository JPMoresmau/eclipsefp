package net.sf.eclipsefp.haskell.buildwrapper;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.osgi.util.NLS;

import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.scion.types.BuildOptions;

public class JobFacade implements IBWFacade {
	private IBWFacade realFacade;
	
	public JobFacade(IBWFacade realF){
		realFacade=realF;
	}
	
	public void build(final BuildOptions buildOptions) {
		final String jobNamePrefix = NLS.bind(BWText.job_build, getProject().getName());

	      Job buildJob = new Job (jobNamePrefix) {
	        @Override
	        protected IStatus run(IProgressMonitor monitor) {
	          try {
	            monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	            realFacade.build(buildOptions);
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

	public void synchronize() {
		final String jobNamePrefix = NLS.bind(BWText.job_synchronize, getProject().getName());

	      Job buildJob = new Job (jobNamePrefix) {
	        @Override
	        protected IStatus run(IProgressMonitor monitor) {
	          try {
	            monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	            realFacade.synchronize();
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
	
	public IProject getProject() {
		return realFacade.getProject();
	}

}
