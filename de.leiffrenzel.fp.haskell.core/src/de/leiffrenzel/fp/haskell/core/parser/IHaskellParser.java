// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.parser;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;


/** <p>specifies a parser for Haskell source files.</p>
  *
  * Note: this is an interim API for experimental purposes. Implementors are
  * declared via the de.leiffrenzel.fp.haskell.core.haskellParsers extension
  * point.
  *  
  * @author Leif Frenzel
  */
public interface IHaskellParser {

  ICompilationUnit parse( final IFile file ) throws CoreException;  

  /** <p>returns whether this <code>IHaskellParser</code> has been properly
    * initialized and can be used for parsing. Clients of the parser API 
    * should call this method to find out whether this 
    * <code>IHaskellParser</code> is safe to use. (There may be native
    * code involved which could be unavailable etc.)</p> */
  boolean canParse();
}