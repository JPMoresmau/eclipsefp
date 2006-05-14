package net.sf.eclipsefp.haskell.core.halamo;

import java.util.Hashtable;
import java.util.Map;

/**
 * A Haskell language model controls relationships between Haskell language
 * elements. It is project-specific and at this time dependencies between
 * projects aren't allowed.
 * 
 * @author Thiago Arrais
 * 
 * @see HaskellModelManager#getModelFor(org.eclipse.core.resources.IProject)
 */
public class HaskellLanguageModel implements IHaskellModel {

	private Map<String, IModule> fModules = new Hashtable<String, IModule>();

	public HaskellLanguageModel() {
		//no need to do anything
	}

	public void putModule(IModule module) {
		fModules.put(module.getName(), module);
	}

	public IModule getModule(String name) {
		return fModules.get(name);
	}

	public Scope getScopeFor(IModule module) {
		return computeScopeFor(module);
	}

	public Scope computeScopeFor(IModule module) {
		Scope result = new Scope();
		addOwnFunctions(module, result);
		addImportedModules(module, result);
		return result;
	}

	private void addImportedModules(IModule module, Scope result) {
		for (IImport imp : module.getImports()) {
			final IModule foreignModule = getModule(imp.getImportedElement());
			result.addAvailableModule(foreignModule);
		}
	}

	private void addOwnFunctions(IModule module, Scope result) {
		result.addAvailableModule(module);
	}
}