package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IImportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

public class Import implements IImport {

	private String fImportedElementName;

	public IModule getModule() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getImportedElement() {
		return fImportedElementName;
	}

	public IImportSpecification[] getImportSpecifications() {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean isHiding() {
		// TODO Auto-generated method stub
		return false;
	}

	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

	public ICompilationUnit getCompilationUnit() {
		// TODO Auto-generated method stub
		return null;
	}

	public IHaskellLanguageElement getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	public ISourceLocation getSourceLocation() {
		// TODO Auto-generated method stub
		return null;
	}

	public void setElementName(String name) {
		fImportedElementName = name;
	}

}
