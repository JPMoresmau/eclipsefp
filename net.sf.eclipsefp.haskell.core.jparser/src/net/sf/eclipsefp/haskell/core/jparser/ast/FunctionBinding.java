package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.IFunctionBinding;
import de.leiffrenzel.fp.haskell.core.halamo.IMatch;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

public class FunctionBinding extends Declaration implements IFunctionBinding {

	public IMatch[] getMatches() {
		return new IMatch[0];
	}

	public IModule getModule() {
		// TODO Auto-generated method stub
		return null;
	}

}
