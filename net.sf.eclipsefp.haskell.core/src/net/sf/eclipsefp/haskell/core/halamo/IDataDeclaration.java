// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

/**
 * <p>represents a data declaration in a module.</p>
 * 
 * <p>
 * These take the form
 * <pre>
 *     data [context =>] simpletype = constrs [deriving]
 * </pre>
 * </p>
 * 
 * <p>
 * Some examples:
 * <pre>
 *     data Tree a = Leaf a | Branch (Tree a) (Tree a)
 *     data Boolean = True | False
 * </pre></p>
 * 
 * @author Leif Frenzel
 */
public interface IDataDeclaration extends IDeclaration {
 
  IConstructor[] getConstructors();
}
