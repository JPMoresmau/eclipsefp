package net.sf.eclipsefp.haskell.core.jparser;

import net.sf.eclipsefp.haskell.core.halamo.ITypeSignature;
import net.sf.eclipsefp.haskell.core.jparser.ast.ClassDeclaration;

class NullClassDeclaration extends ClassDeclaration {

	@Override
	public boolean accepts(final ITypeSignature tsig) {
		return false;
	}

}
