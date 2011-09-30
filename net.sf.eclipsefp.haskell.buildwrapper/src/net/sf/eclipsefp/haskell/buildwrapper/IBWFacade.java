package net.sf.eclipsefp.haskell.buildwrapper;

import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;

import org.eclipse.core.resources.IProject;

@Deprecated
public interface IBWFacade {
	boolean build(BuildOptions buildOptions);
	
	void synchronize(boolean force);

	IProject getProject();
}
