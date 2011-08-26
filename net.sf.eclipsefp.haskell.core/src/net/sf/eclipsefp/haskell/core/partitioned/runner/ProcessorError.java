package net.sf.eclipsefp.haskell.core.partitioned.runner;

/**
 * Represents output returned by a tool, including
 * filename, line and column number and the message.
 *
 * @author Alejandro Serrano
 */
public class ProcessorError {
	private final String filename;
	private final int line;
	private final int column;
	private final String message;

	public ProcessorError(final String filename, final int line,
	    final int column, final String message) {
		this.filename = filename;
		this.line = line;
		this.column = column;
		this.message = message;
	}

	public String getFilename() {
		return filename;
	}

	public int getLine() {
		return line;
	}

	public int getColumn() {
		return column;
	}

	public String getMessage() {
		return message;
	}
}
