package net.sf.eclipsefp.haskell.core.jparser.ast;

import org.eclipse.core.resources.IFile;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

public class CompilationUnit implements ICompilationUnit {

	private IModule fModule;

	public CompilationUnit(IModule module) {
		fModule = module;
	}

	public IModule[] getModules() {
		return new IModule[] { fModule };
	}

	public ISourceLocation getNextLocation(ISourceLocation srcLoc) {
		// TODO Auto-generated method stub
		return null;
	}

	public Object getAdapter(Class adapter) {
		// TODO relax the need for implementing the IAdaptable interface
		// or provide a real implementation for this method
		return null;
	}
	
	public IFile getUnderlyingResource() {
		return null;
	}
	  
}
