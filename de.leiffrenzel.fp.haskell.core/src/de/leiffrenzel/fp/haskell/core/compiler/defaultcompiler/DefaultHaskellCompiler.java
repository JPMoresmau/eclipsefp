// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.compiler.defaultcompiler;

import org.eclipse.core.resources.IFile;

import de.leiffrenzel.fp.haskell.core.compiler.ICompilerOutput;
import de.leiffrenzel.fp.haskell.core.compiler.IHaskellCompiler;
import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;

/** <p>The default implementation for the Haskell compiler. This is a dummy
  * implementation which does actually no compiler calls at all.</p>
  * 
  * @author Leif Frenzel
  */
public class DefaultHaskellCompiler implements IHaskellCompiler {

  /** <p>This default implementation does nothing and returns an output
    * that contains only some basic message.</p> */
  public ICompilerOutput compile( final IFile file ) {
    return new DefaultHaskellCompilerOutput( file.getName() );
  }
  
  public String[] buildCommandLine( final IFile file,
                                    final IHaskellProject haskellProject ) {
    return new String[ 0 ];
  }

  public String getId() {
    return getClass().getName();
  }
}