package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import org.eclipse.core.resources.IFile;

import net.sf.eclipsefp.haskell.core.halamo.Halamo;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.Scope;

public class StubHalamo extends Halamo {

	private Scope fScope;

	public void setModulesInScope(IModule... modules) {
		fScope = new Scope();
		for(IModule module : modules) {
			fScope.addAvailableModule(module);
		}
	}

	@Override
	public Scope getScopeFor(IFile file) {
		return fScope;
	}
	
}
