package net.sf.eclipsefp.haskell.core.compiler;

import java.io.Writer;

public interface ICompilerListener {

	Writer getOutputWriter();

	void startingCompilation();

}
