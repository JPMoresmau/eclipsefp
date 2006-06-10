// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.core.model;

/** <p>represents a stanza in a package description.</p> 
  *
  * @author Leif Frenzel
  */
public abstract class PackageDescriptionStanza {

  private final String name;

  PackageDescriptionStanza( final String name ) {
    this.name = name;
  }
  
  public String getName() {
    return name;
  }
  
  
  // interface methods of Object
  //////////////////////////////
  
  @Override
  public String toString() {
    return getName();
  }
}
