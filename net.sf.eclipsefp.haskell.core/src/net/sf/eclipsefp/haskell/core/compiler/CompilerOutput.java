// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * <p>
 * Default implementation for a compiler's output.
 * </p>
 * 
 * @author Leif Frenzel
 */
public class CompilerOutput implements ICompilerOutput {

	private int exitStatus;

	private String output;

	private final Collection<ICompilerOutputItem> fErrors =
		new ArrayList<ICompilerOutputItem>();

	private List<Exception> exceptions;

	public CompilerOutput(final int exitStatus, final String output,
			final List<Exception> exceptions) {
		this.exitStatus = exitStatus;
		this.output = output;
		this.exceptions = exceptions;
	}

	public CompilerOutput() {
		//placeholder constructor
	}

	@Override
	public String toString() {
		return "Compiler output [ " + exceptions.size() + " Exceptions ]\n"
				+ output + "\n" + fErrors;
	}

	// interface methods of ICompilerOutput
	// ////////////////////////////////////////////

	public int getExitStatus() {
		return exitStatus;
	}

	public String getOutput() {
		return output;
	}

	public Collection<ICompilerOutputItem> getErrors() {
		return new ArrayList<ICompilerOutputItem>(fErrors);
	}

	public List<Exception> getExceptions() {
		return exceptions;
	}

	public void addError(final CompilerOutputItem item) {
		fErrors.add(item);
	}
}