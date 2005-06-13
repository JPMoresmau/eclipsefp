// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/** <p>represents a data declaration in a module.</p>
  *
  * @author Leif Frenzel
  */
public interface IDataDeclaration extends IDeclaration {
 
  IConstructor[] getConstructors();
}
