// Copyright (c) 2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.internal.refactoring.functions;

/** <p>interface for the Haskell function that performs the pointfree 
  * refactoring.</p> 
  *
  * @author Leif Frenzel
  */
public interface IMakePointFree {

  String makePointFree( String content );
}
