package net.sf.eclipsefp.haskell.core.jparser.ast;

import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IImportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IModule;

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
