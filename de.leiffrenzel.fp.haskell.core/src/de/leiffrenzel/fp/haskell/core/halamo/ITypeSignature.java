// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/** <p>represents a type signature declaration in a module.</p>
  *
  * @author Leif Frenzel
  */
public interface ITypeSignature extends IDeclaration {

  /** <p>returns all identifiers for which this type signature is 
    * applied.</p> */
  String[] getIdentifiers();
}
