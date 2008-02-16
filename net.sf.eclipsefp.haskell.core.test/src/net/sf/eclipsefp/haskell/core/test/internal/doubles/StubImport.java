package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IImportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.ISourceLocation;

public class StubImport implements IImport {

	private final String fModule;

	public StubImport(final String module) {
		fModule = module;
	}

	public IImportSpecification[] getImportSpecifications() {
		return null;
	}

	public String getImportedElement() {
		return fModule;
	}

	public IModule getModule() {
		return null;
	}

	public boolean isHiding() {
		return false;
	}

	public ICompilationUnit getCompilationUnit() {
		return null;
	}

	public String getName() {
		return null;
	}

	public IHaskellLanguageElement getParent() {
		return null;
	}

	public ISourceLocation getSourceLocation() {
		return null;
	}
}
