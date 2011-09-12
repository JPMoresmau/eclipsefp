package net.sf.eclipsefp.haskell.buildwrapper;

import org.eclipse.core.resources.IProject;

import net.sf.eclipsefp.haskell.scion.types.BuildOptions;

public interface IBWFacade {
	void build(BuildOptions buildOptions);
	
	void synchronize();
	
	IProject getProject();
}
