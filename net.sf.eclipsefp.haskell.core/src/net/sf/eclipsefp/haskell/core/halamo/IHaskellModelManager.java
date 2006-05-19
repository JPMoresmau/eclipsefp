package net.sf.eclipsefp.haskell.core.halamo;

import org.eclipse.core.resources.IProject;

public interface IHaskellModelManager {
	
	public IHaskellModel getModelFor(IProject project);

}
