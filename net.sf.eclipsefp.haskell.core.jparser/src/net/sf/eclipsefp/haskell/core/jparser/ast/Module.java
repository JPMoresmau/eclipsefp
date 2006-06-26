package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IExportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IModule;

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

	public boolean isEmpty() {
		return    fImports.isEmpty() && fDecls.isEmpty() && fExports.isEmpty()
		       && (getName() == null || getName().length() == 0);
	}

}
