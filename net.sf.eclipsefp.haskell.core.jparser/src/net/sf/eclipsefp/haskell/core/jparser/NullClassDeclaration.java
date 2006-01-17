package net.sf.eclipsefp.haskell.core.jparser;

import de.leiffrenzel.fp.haskell.core.halamo.ITypeSignature;
import net.sf.eclipsefp.haskell.core.jparser.ast.ClassDeclaration;

class NullClassDeclaration extends ClassDeclaration {

	@Override
	public boolean accepts(ITypeSignature tsig) {
		return false;
	}

}
