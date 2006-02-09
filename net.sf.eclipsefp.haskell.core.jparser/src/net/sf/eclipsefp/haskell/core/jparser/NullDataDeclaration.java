package net.sf.eclipsefp.haskell.core.jparser;

import de.leiffrenzel.fp.haskell.core.halamo.IConstructor;
import net.sf.eclipsefp.haskell.core.jparser.ast.DataDeclaration;

class NullDataDeclaration extends DataDeclaration {

	@Override
	public boolean accepts(IConstructor cons) {
		return false;
	}

}
