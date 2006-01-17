package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.IConstructor;
import de.leiffrenzel.fp.haskell.core.halamo.IDataDeclaration;

public class DataDeclaration extends Declaration implements IDataDeclaration {

	private List<IConstructor> fConstructors = new Vector<IConstructor>();

	public IConstructor[] getConstructors() {
		return fConstructors.toArray(new IConstructor[fConstructors.size()]);
	}

	public boolean accepts(IConstructor cons) {
		return true;
	}

	public void addConstructor(IConstructor cons) {
		fConstructors .add(cons);
	}

}
