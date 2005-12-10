// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de>
package de.leiffrenzel.fp.haskell.core.compiler;


/** <p>Default implementation for the information that was parsed out of 
  * the compiler output.</p>
  * 
  * @author Leif Frenzel
  */
public class CompilerOutputItem implements ICompilerOutputItem {

  private String fileName;
  private int lineNumber;
  private String comment;
  
  public CompilerOutputItem( final String fileName,
                             final int lineNumber, 
                             final String comment ) {
    this.fileName = fileName;
    this.lineNumber = lineNumber;
    this.comment = comment.trim();
  }
  
  public void addToComment( final String commentAddition ) {
    String start = ( comment.equals( "" ) ) ? comment : comment + "\n";
    this.comment = start + commentAddition.trim();
  }
  
  public String toString() {
    return   "CompilerOutputItem:"
           + "\n  file   : " + fileName
           + "\n  line   : " + lineNumber
           + "\n  comment: " + comment;
  }
  
  
  // attribute setters and getters
  ////////////////////////////////

  public String getComment() {
    return comment;
  }

  public String getFileName() {
    return fileName;
  }

  public int getLineNumber() {
    return lineNumber;
  }
}