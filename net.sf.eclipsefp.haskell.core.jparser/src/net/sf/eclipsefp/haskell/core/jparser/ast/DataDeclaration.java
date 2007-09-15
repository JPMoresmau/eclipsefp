package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import net.sf.eclipsefp.haskell.core.halamo.IConstructor;
import net.sf.eclipsefp.haskell.core.halamo.IDataDeclaration;

public class DataDeclaration extends Declaration implements IDataDeclaration {

	private final List<IConstructor> fConstructors = new Vector<IConstructor>();

	public IConstructor[] getConstructors() {
		return fConstructors.toArray(new IConstructor[fConstructors.size()]);
	}

	public boolean accepts(final IConstructor cons) {
		return true;
	}

	public void addConstructor(final IConstructor cons) {
		fConstructors .add(cons);
	}

}
