package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IImportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IModule;

public class Import extends HaskellLanguageElement implements IImport {

	private String fImportedElementName;
	private final List<IImportSpecification> fSpecifications = new Vector<IImportSpecification>();
	private boolean fHiding;

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
		return fHiding;
	}

	public void addSpecifications(final List<ImportSpecification> specs) {
		for (ImportSpecification spec : specs) {
			spec.setImport(this);
			fSpecifications.add(spec);
		}
	}

	public void setElementName(final String name) {
		fImportedElementName = name;
		setName(name);
	}

	public void setHiding(final boolean b) {
		fHiding = true;
	}

}
