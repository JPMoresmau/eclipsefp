package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import org.eclipse.core.resources.IFile;

import de.leiffrenzel.fp.haskell.core.halamo.Halamo;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.Scope;

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
