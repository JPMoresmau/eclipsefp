package net.sf.eclipsefp.haskell.core.test.codeassist.doubles;

import net.sf.eclipsefp.haskell.core.codeassist.HaskellCompletionContext;
import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellModel;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.Scope;

public class EmptyCompletionContext extends HaskellCompletionContext {

	public static final class EmptyModel implements IHaskellModel {

		public Scope getScopeFor(IModule module) {
			return new Scope();
		}

		public void putModule(IModule module) {
		}

	}

	public EmptyCompletionContext(ICompilationUnit unit, int offset) {
		super(unit, new EmptyModel(), offset);
	}
	
}
