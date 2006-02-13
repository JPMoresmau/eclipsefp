package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

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
