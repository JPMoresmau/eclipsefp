package net.sf.eclipsefp.haskell.core.halamo;

/**
 * @author Thiago Arrais
 */
public class ScopeCalculator {

	public Scope computeScopeFor(IModule module) {
		Scope result = new Scope();
		addOwnFunctions(module, result);
		addImportedModules(module, result);
		return result;
	}

	private void addImportedModules(IModule module, Scope result) {
		for (IImport imp : module.getImports()) {
			final IModule foreignModule = Halamo.getInstance().getModule(imp.getImportedElement());
			result.addAvailableModule(foreignModule);
		}
	}

	private void addOwnFunctions(IModule module, Scope result) {
		result.addAvailableModule(module);
	}
	
}
