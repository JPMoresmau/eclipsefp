package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import net.sf.eclipsefp.haskell.core.halamo.IExportThingWith;
import net.sf.eclipsefp.haskell.core.halamo.IExportThingWithComponent;

public class ExportThingWith extends ExportSpecification implements
		IExportThingWith {

	private final List<IExportThingWithComponent> fComponents =
		new Vector<IExportThingWithComponent>();
	
	public IExportThingWithComponent[] getComponents() {
		return fComponents.toArray(
				new IExportThingWithComponent[fComponents.size()]);
	}

	public void addComponent(final ExportThingWithComponent component) {
		fComponents.add(component);
	}

}
