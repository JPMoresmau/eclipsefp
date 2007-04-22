package net.sf.eclipsefp.haskell.core.test.compiler;

import java.io.IOException;
import java.io.Writer;

import net.sf.eclipsefp.haskell.core.compiler.DefaultHaskellCompiler;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;

import org.eclipse.core.resources.IFile;

public class StubCompiler extends DefaultHaskellCompiler {

	public static final String EXPECTED_STANDARD_ERROR = "expected standard error output";
	public static final String EXPECTED_STANDARD_OUTPUT = "expected standard output";

	public ICompilerOutput compile(IFile workingDir, Writer output) {
		try {
			output.write(EXPECTED_STANDARD_OUTPUT);
			output.write(EXPECTED_STANDARD_ERROR);
			output.flush();
		} catch (IOException e) {
			// ignore error
		}
		return null;
	}

}
