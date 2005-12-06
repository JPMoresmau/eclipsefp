package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.ITypeSignature;

public class TypeSignature extends Declaration implements ITypeSignature {

	public String[] getIdentifiers() {
		return new String[] { getName() };
	}

}
