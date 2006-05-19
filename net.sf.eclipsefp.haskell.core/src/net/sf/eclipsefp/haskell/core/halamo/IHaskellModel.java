package net.sf.eclipsefp.haskell.core.halamo;

public interface IHaskellModel {

	void putModule(IModule module);
	
	Scope getScopeFor(IModule module);	

}
