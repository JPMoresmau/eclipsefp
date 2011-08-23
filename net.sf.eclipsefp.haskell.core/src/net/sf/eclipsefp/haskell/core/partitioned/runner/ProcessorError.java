package net.sf.eclipsefp.haskell.core.partitioned.runner;

public class ProcessorError {
	private String filename;
	private int line;
	private int column;
	private String message;
	
	public ProcessorError(String filename, int line, int column, String message) {
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
