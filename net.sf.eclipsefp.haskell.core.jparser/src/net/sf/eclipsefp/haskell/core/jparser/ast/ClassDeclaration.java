package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import net.sf.eclipsefp.haskell.core.halamo.IClassDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.ITypeSignature;

public class ClassDeclaration extends Declaration implements IClassDeclaration {

	private List<ITypeSignature> fSignatures = new Vector<ITypeSignature>();

	public ITypeSignature[] getTypeSignatures() {
		return fSignatures.toArray(new ITypeSignature[fSignatures.size()]);
	}

	public boolean accepts(ITypeSignature tsig) {
		return true;
	}

	public void addTypeSignature(ITypeSignature tsig) {
		fSignatures.add(tsig);
	}

}
