package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IImportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

public class Module extends HaskellLanguageElement implements IModule {

	private List<IExportSpecification> fExports;
	private List<IImport> fImports;
	private List<IDeclaration> fDecls;

	public Module() {
		fExports = new Vector<IExportSpecification>();
		fImports = new Vector<IImport>();
		fDecls = new Vector<IDeclaration>();
	}
	
	public IExportSpecification[] getExportSpecifications() {
		return fExports.toArray(new IExportSpecification[fExports.size()]);
	}

	public IImport[] getImports() {
		return fImports.toArray(new IImport[fImports.size()]);
	}

	public IDeclaration[] getDeclarations() {
		return fDecls.toArray(new IDeclaration[fDecls.size()]);
	}

	public ICompilationUnit getCompilationUnit() {
		// TODO Auto-generated method stub
		return null;
	}

	public void addDeclaration(IDeclaration decl) {
		fDecls.add(decl);
	}

	public void addExport(IExportSpecification export) {
		fExports.add(export);
	}

	public void addImport(IImport theImport) {
		fImports.add(theImport);
	}

}
