package net.sf.eclipsefp.haskell.core.halamo;

import java.util.*;

public class Scope {

	public static final Scope EMPTY = new Scope();
	
	private IHaskellModel fModel;
	private List<IModule> fAvailableModules = new ArrayList<IModule>();

	/**
	 * Creates an empty scope
	 */
	public Scope() {
		//placeholder constructor
	}
	
	/**
	 * Creates a scope inside the given model
	 * @param module 
	 * 
	 * @param model	to be used when 
	 */
	public Scope(IModule module, IHaskellModel model) {
		fModel = model;
		addImportedModules(module);
	}
	
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

	private void addImportedModules(IModule module) {
		for (IImport imp : module.getImports()) {
			final IModule foreignModule = fModel.getModule(imp.getImportedElement());
			addAvailableModule(foreignModule);
		}
	}
	
}
