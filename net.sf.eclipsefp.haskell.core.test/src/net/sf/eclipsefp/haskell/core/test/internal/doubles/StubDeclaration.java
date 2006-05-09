package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.ISourceLocation;

public class StubDeclaration implements IDeclaration {

	private String fName;

	public StubDeclaration(String name) {
		fName = name;
	}

	public IModule getModule() {
		return null;
	}

	public String getName() {
		return fName;
	}

	public ICompilationUnit getCompilationUnit() {
		return null;
	}

	public IHaskellLanguageElement getParent() {
		return null;
	}

	public ISourceLocation getSourceLocation() {
		return null;
	}

}
