package net.sf.eclipsefp.haskell.buildwrapper;

import net.sf.eclipsefp.haskell.scion.types.BuildOptions;

import org.eclipse.core.resources.IProject;

public interface IBWFacade {
	void build(BuildOptions buildOptions);
	
	void synchronize();

	IProject getProject();
}
