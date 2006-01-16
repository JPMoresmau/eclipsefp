package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IImportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

public class ImportSpecification extends HaskellLanguageElement implements IImportSpecification {

	private IImport fImport;

	public IModule getModule() {
		// TODO Auto-generated method stub
		return null;
	}

	public IImport getImport() {
		return fImport;
	}

	public void setImport(Import imp) {
		fImport = imp;
	}

}
