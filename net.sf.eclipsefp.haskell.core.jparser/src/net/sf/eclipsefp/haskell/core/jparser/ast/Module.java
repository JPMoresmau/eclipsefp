package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

public class Module implements IModule {

	private String fName;
	private List<IExportSpecification> fExports;
	private IImport[] fImports;
	private IDeclaration[] fDecls;

	public Module() {
		fName = "";
		fExports = new Vector<IExportSpecification>();
		fImports = new IImport[0];
		fDecls = new IDeclaration[0];
	}
	
	public IExportSpecification[] getExportSpecifications() {
		return fExports.toArray(new IExportSpecification[fExports.size()]);
	}

	public IImport[] getImports() {
		return fImports;
	}

	public IDeclaration[] getDeclarations() {
		return fDecls;
	}

	public ICompilationUnit getCompilationUnit() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getName() {
		return fName;
	}

	public void setName(String s) {
		fName = s;
	}

	public IHaskellLanguageElement getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	public ISourceLocation getSourceLocation() {
		// TODO Auto-generated method stub
		return null;
	}

	public void addExports(List<IExportSpecification> someExports) {
		fExports.addAll(someExports);
	}

}
