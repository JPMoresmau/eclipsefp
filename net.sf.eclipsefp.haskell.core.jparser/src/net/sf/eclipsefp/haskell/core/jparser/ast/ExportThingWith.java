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

	public void addComponents(final List<String> exportComponents) {
		for (String component : exportComponents) {
			fComponents.add(new ExportComponent(component));
		}
	}
	
	private class ExportComponent implements IExportThingWithComponent {

		private String fName;

		public ExportComponent(String name) {
			fName = name;
		}

		public IExportThingWith getExportSpecification() {
			return ExportThingWith.this;
		}

		public String getName() {
			return fName;
		}

		public ICompilationUnit getCompilationUnit() {
			// TODO Auto-generated method stub
			return null;
		}

		public IHaskellLanguageElement getParent() {
			return ExportThingWith.this;
		}

		public ISourceLocation getSourceLocation() {
			return null;
		}
		
	}

}
