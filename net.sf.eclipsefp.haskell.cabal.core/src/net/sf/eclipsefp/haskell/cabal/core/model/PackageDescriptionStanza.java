// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.core.model;

/** <p>represents a stanza in a package description.</p> 
  *
  * @author Leif Frenzel
  */
public abstract class PackageDescriptionStanza {

  private final String name;
  private final int startLine;
  private final int endLine;

  PackageDescriptionStanza( final String name, 
                            final int startLine, 
                            final int endLine ) {
    this.name = name;
    this.startLine = startLine;
    this.endLine = endLine;
  }
  
  public String getName() {
    return name;
  }
  
  public int getStartLine() {
    return startLine;
  }
  
  public int getEndLine() {
    return endLine;
  }
  
  
  // interface methods of Object
  //////////////////////////////
  
  @Override
  public String toString() {
    return getName() + " (line " + startLine + "-" + endLine + ")";
  }
}
