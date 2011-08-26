/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

/**
 * Represents a filename + line + column.
 * @author Alejandro Serrano
 *
 */
public class SourceLocation {
	private String filename;
	private int line;
	private int column;

	public SourceLocation(String filename, int line, int column) {
		this.filename = filename;
		this.line = line;
		this.column = column;
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

	public static SourceLocation fromString(String info) {
		int colonLoc = info.indexOf(':');
		if (colonLoc == -1) {
			// We don't have line/column info
			return new SourceLocation(info, -1, -1);
		}

		String filename = info.substring(0, colonLoc);
		String rest = info.substring(colonLoc + 1);
		int secondColonLoc = rest.indexOf(':');
		if (secondColonLoc == -1) {
			// We only have line info
			return new SourceLocation(filename, Integer.parseInt(rest), -1);
		}

		String line = rest.substring(0, secondColonLoc);
		String column = rest.substring(secondColonLoc + 1);
		return new SourceLocation(filename, Integer.parseInt(line), Integer.parseInt(column));
	}
}
