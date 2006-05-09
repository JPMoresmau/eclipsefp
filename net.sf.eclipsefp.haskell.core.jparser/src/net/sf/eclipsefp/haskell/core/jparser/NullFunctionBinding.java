package net.sf.eclipsefp.haskell.core.jparser;

import net.sf.eclipsefp.haskell.core.halamo.IMatch;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionBinding;

public class NullFunctionBinding extends FunctionBinding {

	@Override
	public boolean acceptsMatch(IMatch match) {
		return false;
	}
	
}
