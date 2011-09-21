package net.sf.eclipsefp.haskell.buildwrapper;

import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Map;

import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;

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
			BuildWrapperPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
		}

	}

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

	public List<Component> getComponents() {
		// TODO Auto-generated method stub
		return null;
	}
	
	public Map<String, CabalPackage[]> getPackagesByDB() {
		// TODO Auto-generated method stub
		return null;
	}
	
}
