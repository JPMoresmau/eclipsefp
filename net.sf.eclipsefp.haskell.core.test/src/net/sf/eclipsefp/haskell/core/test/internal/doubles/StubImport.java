package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IImportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.ISourceLocation;

public class StubImport implements IImport {

	private String fModule;

	public StubImport(String module) {
		fModule = module;
	}

	public IImportSpecification[] getImportSpecifications() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getImportedElement() {
		return fModule;
	}

	public IModule getModule() {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean isHiding() {
		// TODO Auto-generated method stub
		return false;
	}

	public ICompilationUnit getCompilationUnit() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getName() {
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

}
