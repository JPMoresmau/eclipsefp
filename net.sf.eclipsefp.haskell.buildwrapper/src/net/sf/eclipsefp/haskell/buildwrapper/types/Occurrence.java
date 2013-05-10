// Copyright (c) 2007 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

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
  
  /**
   * build an occurrence with a guaranteed length calculated from the doc
   * @param td
   * @param doc
   * @throws BadLocationException
   */
  public Occurrence(TokenDef td,IDocument doc) throws BadLocationException{
	  this.line=td.getLocation().getStartLine();
	  this.column=td.getLocation().getStartColumn();
	  this.length=td.getLocation().getLength(doc);
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
