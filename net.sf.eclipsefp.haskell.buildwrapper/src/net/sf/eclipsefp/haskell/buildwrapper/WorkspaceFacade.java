package net.sf.eclipsefp.haskell.buildwrapper;

import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.types.BuildOptions;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.actions.WorkspaceModifyOperation;

public class WorkspaceFacade implements IBWFacade {
	private IBWFacade realFacade;
	private IProgressMonitor monitor;
	
	public WorkspaceFacade(IBWFacade realF,IProgressMonitor monitor){
		realFacade=realF;
		this.monitor=monitor;
	}
	
	public void build(final BuildOptions buildOptions) {
		WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(getProject()){
	    	@Override
	    	protected void execute(IProgressMonitor arg0) throws CoreException,
	    			InvocationTargetException, InterruptedException {
	    		 realFacade.build(buildOptions);
	    	}
	    };
		try {
			wmo.run(monitor);
		} catch (InterruptedException ie){
			// noop
		}catch (InvocationTargetException ie){
			ScionPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
		}

	}

	public IProject getProject() {
		return realFacade.getProject();
	}

	public void synchronize() {
		WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(getProject()){
	    	@Override
	    	protected void execute(IProgressMonitor arg0) throws CoreException,
	    			InvocationTargetException, InterruptedException {
	    		 realFacade.synchronize();
	    	}
	    };
		try {
			wmo.run(monitor);
		} catch (InterruptedException ie){
			// noop
		}catch (InvocationTargetException ie){
			ScionPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
		}
	}

}
