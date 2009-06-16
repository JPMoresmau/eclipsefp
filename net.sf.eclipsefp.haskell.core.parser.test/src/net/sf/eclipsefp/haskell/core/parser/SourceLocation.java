// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import net.sf.eclipsefp.haskell.core.halamo.ISourceLocation;

/** <p>implementation for the <code>ISourceLocation</code> interface.</p>
  *
  * @author Leif Frenzel
  */
public class SourceLocation implements ISourceLocation {

  private final int line;
  private final int column;

  public SourceLocation( final int line, final int column ) {
    // in the Eclipse (JFace Text) world text counting is zero-based,
    // what we get from this native parser is natural counting, we convert
    // here, once and for all
    this.line = line - 1;
    this.column = column - 1;
  }
  
  
  // interface methods of Object
  //////////////////////////////
  
  @Override
public String toString() {
    return "(" + line + ", " + column + ")";
  }
  
  
  // interface methods of ISourceLocation
  ///////////////////////////////////////
  
  public int getLine() {
    return line;
  }

  public int getColumn() {
    return column;
  }
  
  public boolean isAfter( final ISourceLocation anotherLocation ) {
    return    line > anotherLocation.getLine() 
           || (     line == anotherLocation.getLine() 
                 && column > anotherLocation.getColumn() );    
  }
  
  public boolean isBefore( final ISourceLocation anotherLocation ) {
    return     !isAfter( anotherLocation) 
            && !isEqual( anotherLocation );
  }


  // helping methods
  //////////////////
  
  private boolean isEqual( ISourceLocation anotherLocation ) {
    return    line == anotherLocation.getLine()
           && column == anotherLocation.getColumn();
  }


  public long getOffset() {
    return 0;
  }
}
