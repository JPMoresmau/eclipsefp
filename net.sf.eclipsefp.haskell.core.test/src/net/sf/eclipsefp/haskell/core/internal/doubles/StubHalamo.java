package net.sf.eclipsefp.haskell.core.internal.doubles;

import net.sf.eclipsefp.haskell.core.halamo.HaskellLanguageModel;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.Scope;

public class StubHalamo extends HaskellLanguageModel {

	private Scope fScope = Scope.EMPTY;

	public void setModulesInScope(final IModule... modules) {
		fScope = new Scope();
		for(IModule module : modules) {
			fScope.addAvailableModule(module);
		}
	}

	@Override
	public Scope getScopeFor(final IModule module) {
		Scope scope = new Scope(module, this);
		scope.addAvailableModules(fScope.getAvailableModules());
		return scope;
	}

}
