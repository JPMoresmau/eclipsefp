// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import org.eclipse.core.runtime.*;

/** <p>Default implementation of IImportLibray for internal use in the 
  * core.</p>
  * 
  * @author Leif Frenzel
  */
class ImportLibrary implements IImportLibrary {

  private IPath path;
  private boolean used;

  ImportLibrary( final IPath path, final boolean used ) {
    this.path = path;
    this.used = used;
  }

  
  // interface methods of java.lang.Object
  ////////////////////////////////////////
  
  public boolean equals( final Object obj ) {
    boolean result = false;
    if( obj instanceof ImportLibrary ) {
      ImportLibrary object = ( ImportLibrary )obj;
      result = object.getPath().equals( path );
    }
    return result;
  }

  public String toString() {
    return path.toOSString();
  }
 
  
  // interface methods of IImportLibrary
  //////////////////////////////////////
  
  public IPath getPath() {
    return path;
  }

  public void setPath( final IPath path ) {
    this.path = path;
  }

  public boolean isUsed() {
    return used;
  }

  public void setUsed( final boolean enabled ) {
    this.used = enabled;
  }
}