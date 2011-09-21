package net.sf.eclipsefp.haskell.buildwrapper;

import java.util.List;

import net.sf.eclipsefp.haskell.buildwrapper.types.BWTarget;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineHandler;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.osgi.util.NLS;

public class JobFacade implements IBWFacade {
	private BWFacade realFacade;
	
	public JobFacade(BWFacade realF){
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
	            List<OutlineDef> defs=realFacade.outline(f);
	            handler.handleOutline(defs);
	          } finally {
	            monitor.done();
	          }
	          return Status.OK_STATUS;
	        }
	      };
	      buildJob.setRule( getProject() );
	      buildJob.setPriority(Job.INTERACTIVE);
	      buildJob.schedule();
	}
	
	public void updateFromEditor(final IFile f,final IDocument doc,final OutlineHandler handler){
		final String jobNamePrefix = NLS.bind(BWText.editor_job_name, getProject().getName());
	
	    Job buildJob = new Job (jobNamePrefix) {
	      @Override
	      protected IStatus run(IProgressMonitor monitor) {
	        try {
	          monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
	          realFacade.write(f, doc.get());
	          List<OutlineDef> defs=realFacade.outline(f);
	          handler.handleOutline(defs);
	          realFacade.build(new BuildOptions().setOutput(false).setTarget(BWTarget.Target).setConfigure(false).setRecompile(false));
	        } finally {
	          monitor.done();
	        }
	        return Status.OK_STATUS;
	      }
	    };
	    buildJob.setRule( getProject() );
	    buildJob.setPriority(Job.INTERACTIVE);
	    buildJob.schedule();
	}
	
	public IProject getProject() {
		return realFacade.getProject();
	}

}
