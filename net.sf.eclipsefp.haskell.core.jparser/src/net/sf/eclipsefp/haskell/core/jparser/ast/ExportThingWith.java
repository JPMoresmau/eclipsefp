package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IExportThingWith;
import de.leiffrenzel.fp.haskell.core.halamo.IExportThingWithComponent;
import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

public class ExportThingWith extends ExportSpecification implements
		IExportThingWith {

	private List<IExportThingWithComponent> fComponents =
		new Vector<IExportThingWithComponent>();
	
	public IExportThingWithComponent[] getComponents() {
		return fComponents.toArray(
				new IExportThingWithComponent[fComponents.size()]);
	}

	public void addComponent(ExportThingWithComponent component) {
		fComponents.add(component);
	}

}
