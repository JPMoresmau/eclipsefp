// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/** <p>represents a function binding in a module.</p>
  *
  * @author Leif Frenzel
  */
public interface IFunctionBinding extends IDeclaration {

  /** <p>returns the clauses in this function binding.</p> */
  IMatch[] getMatches();
}
