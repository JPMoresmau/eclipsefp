// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/** <p>A source location of a language element, represented as line and
  * column in the source file.</p>
  * 
  * @author Leif Frenzel
  */
public interface ISourceLocation {

  /** <p>the line in the source file.</p> */
  int getLine();
  /** <p>the column in the source file.</p> */
  int getColumn();
  
  // ordering
  ///////////
  
  /** <p>whether this <code>ISourceLocation</code> is before the specified
    * other location.</p> */
  boolean isBefore( ISourceLocation anotherLocation );
  /** <p>whether this <code>ISourceLocation</code> is after the specified
   * other location.</p> */
  boolean isAfter( ISourceLocation anotherLocation );
}
