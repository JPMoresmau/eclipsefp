// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import org.eclipse.core.runtime.IPath;

/** <p>represents an imported library.</p>
  * 
  * @author Leif Frenzel
  */
public interface IImportLibrary {

  IPath getPath();

  void setPath( IPath path );

  boolean isUsed();

  void setUsed( boolean enabled );
}