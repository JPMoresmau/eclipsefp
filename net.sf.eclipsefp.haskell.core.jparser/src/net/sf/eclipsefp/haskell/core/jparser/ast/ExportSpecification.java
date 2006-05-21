package net.sf.eclipsefp.haskell.core.jparser.ast;

import net.sf.eclipsefp.haskell.core.halamo.IExportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IModule;

public class ExportSpecification extends HaskellLanguageElement implements IExportSpecification {

	public IModule getModule() {
		// TODO check this method's users to remove it
		return null;
	}

}
