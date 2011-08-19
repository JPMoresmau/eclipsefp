package net.sf.eclipsefp.haskell.partitioned;

public class Error {
	private String filename;
	private int line;
	private int column;
	private String message;
	
	public Error(String filename, int line, int column, String message) {
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
