package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IImportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

public class Import implements IImport {

	private String fImportedElementName;
	private SourceLocation fLocation = new SourceLocation();
	private List<IImportSpecification> fSpecifications = new Vector<IImportSpecification>();

	public IModule getModule() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getImportedElement() {
		return fImportedElementName;
	}

	public IImportSpecification[] getImportSpecifications() {
		return fSpecifications.toArray(new IImportSpecification[fSpecifications.size()]);
	}

	public boolean isHiding() {
		// TODO Auto-generated method stub
		return false;
	}

	public String getName() {
		return fImportedElementName;
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
		return fLocation;
	}

	public void setElementName(String name) {
		fImportedElementName = name;
	}

	public void setLocation(int line, int column) {
		fLocation.setPoint(line, column);
	}

	public void addSpecifications(List<IImportSpecification> specs) {
		fSpecifications.addAll(specs);
	}

}
