package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

public class SourceLocation implements ISourceLocation {

	private int fLine;
	private int fColumn;
	private long fOffset;

	public SourceLocation() {
	}

	public SourceLocation(int line, int column) {
		setPoint(line, column);
	}

	public int getLine() {
		return fLine;
	}

	public int getColumn() {
		return fColumn;
	}

	public boolean isAfter(final ISourceLocation anotherLocation) {
		return    fLine > anotherLocation.getLine() 
		       || (  fLine == anotherLocation.getLine() 
			      && fColumn > anotherLocation.getColumn() );    
	}
	
	public boolean isBefore(final ISourceLocation anotherLocation) {
		return !isAfter( anotherLocation) 
		    && !isEqual( anotherLocation );
	}
	
	
    public void setPoint(int line, int column) {
		fLine = line;
		fColumn = column;
	}

	public void setOffset(long offset) {
		fOffset = offset;
	}

	public long getOffset() {
		return fOffset;
	}
	
	// helping methods
	//////////////////
	
	private boolean isEqual( ISourceLocation anotherLocation ) {
		return fLine == anotherLocation.getLine()
		    && fColumn == anotherLocation.getColumn();
	}

}
