// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.core;

/** <p>an entry for the interface list.</p>
  *
  * @author Leif Frenzel
  */
public class InterfaceListEntry {

  private final String fileName;
  private boolean used;
  
  
  public InterfaceListEntry( final String fileName, final boolean used ) {
    this.fileName = fileName;
    this.used = used;
  }
  
  
  // attribute setters and getters
  ////////////////////////////////
  
  public String getFileName() {
    return fileName;
  }
  
  public boolean isUsed() {
    return used;
  }
  
  public void setUsed( final boolean used ) {
    this.used = used;
  }
}
