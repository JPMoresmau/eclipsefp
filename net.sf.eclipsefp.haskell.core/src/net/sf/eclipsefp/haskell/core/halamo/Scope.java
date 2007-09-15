package net.sf.eclipsefp.haskell.core.halamo;

import java.util.*;

public class Scope {

	public static final Scope EMPTY = new Scope();
	
	private IHaskellModel fModel;
	private IModule fModule = new NullModule();
	private final List<IModule> fAvailableModules = new ArrayList<IModule>();

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
	public Scope(final IModule module, final IHaskellModel model) {
		fModel = model;
		fModule = module;
		addImportedModules(module);
	}
	
	public List<IModule> getAvailableModules() {
		return new ArrayList<IModule>(fAvailableModules);
	}

	public void addAvailableModule(final IModule mod) {
		fAvailableModules.add(mod);
	}

	public void addAvailableModules(final List<IModule> modules) {
		fAvailableModules.addAll(modules);
	}
	
	public List<IDeclaration> getAvailableDeclarations() {
		List<IDeclaration> decls = new ArrayList<IDeclaration>();
		decls.addAll(Arrays.asList(fModule.getDeclarations()));
		for(IModule module : fAvailableModules) {
			decls.addAll(Arrays.asList(module.getDeclarations()));
		}
		return decls;
	}

	private void addImportedModules(final IModule module) {
		for (IImport imp : module.getImports()) {
			final IModule foreignModule = fModel.getModule(imp.getImportedElement());
			if (foreignModule != null) {
				addAvailableModule(foreignModule);
			}
		}
	}

	public Collection<IModule> getImportableModules() {
		return fModel.getModules();
	}

	/**
	 * Returns all the declarations that actually create something (a data type,
	 * function, class, etc.). Type signatures, for example, do not create
	 * anything.
	 * 
	 * @return list of creating declarations
	 */
	public List<IDeclaration> getCreatingDeclarations() {
		List<IDeclaration> decls = getAvailableDeclarations();
		List<IDeclaration> result = new ArrayList<IDeclaration>(decls.size());
		for(IDeclaration decl : decls) {
			if (! (decl instanceof ITypeSignature)) {
				result.add(decl);
			}
		}
		return result;
	}

}
