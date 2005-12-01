package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.IClassDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.ITypeSignature;

public class ClassDeclaration extends Declaration implements IClassDeclaration {

	public ITypeSignature[] getTypeSignatures() {
		return new ITypeSignature[0];
	}

}
