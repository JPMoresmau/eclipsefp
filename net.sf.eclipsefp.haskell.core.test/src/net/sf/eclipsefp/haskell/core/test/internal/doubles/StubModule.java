package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IExportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.ISourceLocation;

public class StubModule implements IModule {

	private IDeclaration[] fDeclarations;

	public StubModule(String... declNames) {
		fDeclarations = new IDeclaration[declNames.length];
		for(int i = 0; i < declNames.length; ++i) {
			fDeclarations[i] = new StubDeclaration(declNames[i]);
		}
	}

	public IExportSpecification[] getExportSpecifications() {
		return null;
	}

	public IImport[] getImports() {
		return null;
	}

	public IDeclaration[] getDeclarations() {
		return fDeclarations;
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
