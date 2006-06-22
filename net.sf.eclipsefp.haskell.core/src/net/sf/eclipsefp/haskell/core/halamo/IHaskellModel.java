package net.sf.eclipsefp.haskell.core.halamo;

import java.util.Collection;

public interface IHaskellModel {

	void putModule(IModule module);
	void removeModule(String name);	
	
	Scope getScopeFor(IModule module);
	IModule getModule(String string);
	Collection<IModule> getModules();

}
