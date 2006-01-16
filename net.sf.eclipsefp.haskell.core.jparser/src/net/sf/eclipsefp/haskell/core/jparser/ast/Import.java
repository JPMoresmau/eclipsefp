package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IImportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

public class Import extends HaskellLanguageElement implements IImport {

	private String fImportedElementName;
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

	public void addSpecifications(List<ImportSpecification> specs) {
		for (ImportSpecification spec : specs) {
			spec.setImport(this);
			fSpecifications.add(spec);
		}
	}

	public void setElementName(String name) {
		fImportedElementName = name;
		setName(name);
	}

}
