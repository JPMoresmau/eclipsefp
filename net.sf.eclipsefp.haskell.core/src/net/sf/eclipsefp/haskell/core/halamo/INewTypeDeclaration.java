// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

/**
 * <p>Represents a newtype declaration,a datatype renaming, in a module.</p>
 * 
 * <p>
 * These are of the form:
 * <pre>
 *     newtype [context =>] simpletype = newconstr [deriving]
 * </pre>
 * </p>
 * 
 * <p>
 * Some examples:
 * <pre>
 *     newtype N = N Int
 *     newtype Age = Age { unAge :: Int }
 * </pre>
 * </p>
 *
 * @author Leif Frenzel
 */
public interface INewTypeDeclaration extends IDeclaration {

  //no specialities
}
