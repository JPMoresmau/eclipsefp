// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

/** <p>Represents what the compiler had to say about one particular problem
  * at a line in a file. These items are used for displaying compiler outputs
  * in tables, e.g. on the Tasks or Problem viewer.</p>
  * 
  * @author Leif Frenzel
  */
public interface ICompilerOutputItem {

  /** <p>constant for compiler output items global for a resource,  
    * which have no line number.</p> */
  int LINE_UNSPECIFIED = -1;
  
  int getLineNumber();
  
  String getFileName();
  
  String getComment();

}