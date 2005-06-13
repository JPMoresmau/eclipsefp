// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/** <p>represents a class declaration in a module.</p>
  *
  * @author Leif Frenzel
  */
public interface IClassDeclaration extends IDeclaration {

  /** <p>returns the type signatures for this class declaration.</p> */
  ITypeSignature[] getTypeSignatures();
}
