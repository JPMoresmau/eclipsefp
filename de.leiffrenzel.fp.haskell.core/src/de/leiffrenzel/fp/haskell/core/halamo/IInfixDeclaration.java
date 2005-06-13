// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/** <p>represents an infix declaration in a module.</p>
  *
  * @author Leif Frenzel
  */
public interface IInfixDeclaration extends IDeclaration {

  /** <p>none-associative</p> */
  int ASSOC_NONE  = 0;
  /** <p>left-associative</p> */
  int ASSOC_LEFT  = 1;
  /** <p>right-associative</p> */
  int ASSOC_RIGHT = 2;
  
  /** <p>returns the associativity declared in this infix declaration,
    * this must be one of the constants in 
    * <code>IInfixDeclaration</code>.</p> */
  int getAssociativity();
  /** <p>returns the precedence level declared in this infix declaration. This 
    * must be an integer between 0 and 9.</p> */
  int getPrecedenceLevel();
  /** <p>returns the operators for which the fixity is declared in this infix
    * declaration.</p> */
  String[] getOperators();
}
