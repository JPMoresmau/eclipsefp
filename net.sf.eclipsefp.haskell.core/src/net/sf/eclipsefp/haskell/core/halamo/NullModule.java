package net.sf.eclipsefp.haskell.core.halamo;

public class NullModule implements IModule {

	public ICompilationUnit getCompilationUnit() {
		return null;
	}

	public IDeclaration[] getDeclarations() {
		return new IDeclaration[0];
	}

	public IExportSpecification[] getExportSpecifications() {
		return null;
	}

	public IImport[] getImports() {
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
