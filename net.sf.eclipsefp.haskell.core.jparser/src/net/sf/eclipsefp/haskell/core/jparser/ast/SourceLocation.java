package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

public class SourceLocation implements ISourceLocation {

	private int fLine;
	private int fColumn;

	public int getLine() {
		return fLine;
	}

	public int getColumn() {
		return fColumn;
	}

	public boolean isBefore(ISourceLocation anotherLocation) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isAfter(ISourceLocation anotherLocation) {
		// TODO Auto-generated method stub
		return false;
	}

	public void setPoint(int line, int column) {
		fLine = line;
		fColumn = column;
	}

}
