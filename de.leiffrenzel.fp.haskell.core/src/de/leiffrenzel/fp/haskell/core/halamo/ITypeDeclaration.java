// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/**
 * <p>Represents a type declaration in a module.</p>
 * 
 * <p>
 * These take the form
 * <pre>
 *     type simpletype = type
 * </pre>
 * </p>
 * 
 * <p>
 * Some examples are
 * <pre>
 *     type String = [Char]
 *     type Name = String
 * </pre>
 * </p>
 *
 * @author Leif Frenzel
 */
public interface ITypeDeclaration extends IDeclaration {
  
  //no specialities
}
