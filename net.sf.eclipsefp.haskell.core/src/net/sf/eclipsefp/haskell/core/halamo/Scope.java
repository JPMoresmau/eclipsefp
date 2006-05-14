package net.sf.eclipsefp.haskell.core.halamo;

import java.util.*;

public class Scope {

	public static final Scope EMPTY = new Scope();
	
	private List<IModule> fAvailableModules = new ArrayList<IModule>();
	
	public List<IModule> getAvailableModules() {
		return new ArrayList<IModule>(fAvailableModules);
	}

	public void addAvailableModule(IModule mod) {
		fAvailableModules.add(mod);
	}

	public List<IDeclaration> getAvailableDeclarations() {
		List<IDeclaration> decls = new ArrayList<IDeclaration>();
		for(IModule module : fAvailableModules) {
			decls.addAll(Arrays.asList(module.getDeclarations()));
		}
		return decls;
	}
	
}
