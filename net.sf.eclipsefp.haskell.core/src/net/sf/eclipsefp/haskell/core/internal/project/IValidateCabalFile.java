// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

import org.eclipse.core.resources.IFile;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;

/** <p>interface to the Haskell function for Cabal file validation.</p>
  *
  * @author Leif Frenzel
  */
public interface IValidateCabalFile {

  void validate( IFile cabalFile ) throws CohatoeException;
}
