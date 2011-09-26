// Copyright (c) 2007 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.buildwrapper.types;

public class Occurrence {

  private final int line;
  private final int column;
  private final int length;

  public Occurrence( final int line, final int column, final int length ) {
    this.line = line;
    this.column = column;
    this.length = length;
  }

  public Occurrence(TokenDef td){
	  this.line=td.getLocation().getStartLine();
	  this.column=td.getLocation().getStartColumn();
	  this.length=td.getLocation().getEndColumn()-td.getLocation().getStartColumn();
  }

  // attribute getters
  ////////////////////

  public int getLine() {
    return line;
  }

  public int getColumn() {
    return column;
  }

  public int getLength() {
    return length;
  }
}
