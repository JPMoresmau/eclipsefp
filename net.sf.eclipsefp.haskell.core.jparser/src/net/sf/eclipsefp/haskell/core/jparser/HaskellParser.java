package net.sf.eclipsefp.haskell.core.jparser;

import java.io.Reader;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;

public class HaskellParser {

	public ICompilationUnit parse(Reader reader) {
		return new CompilationUnit();
	}

}
