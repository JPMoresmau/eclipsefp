// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.compiler;

import org.eclipse.core.resources.IFile;

import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;

/** <p>Implementations know how to compile a haskell file.</p>
  * 
  * @author Leif Frenzel
  */
public interface IHaskellCompiler {
  
  /** <p>compiles the specified file.</p> */
  ICompilerOutput compile( IFile file );
  /** <p>returns the command line for compiling the specified file.</p> */
  String[] buildCommandLine( IFile file, IHaskellProject haskellProject );

}