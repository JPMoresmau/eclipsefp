// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

/** <p>A compiler output listener implements code that is to be executed
  * when a compiler has produced output. Implementing classes can register
  * themselves with the CompilerManager:</p>
  * 
  * <pre>
  *   ICompilerOutputListener coli = ...
  *   CompilerManager.getInstance().addCompilerOutputListener( coli );
  * </pre>
  * 
  * <p>Don't forget to remove a listener if it is no longer needed!</p>
  * 
  * @author Leif Frenzel
  */
public interface ICompilerOutputListener {

  /** <p>called when compiler output has been produced.</p> */
  void outputProduced( ICompilerOutput producedOutput ); 

}
