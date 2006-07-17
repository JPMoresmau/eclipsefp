// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.util.Collection;
import java.util.List;

/** <p>The result of a call to IHaskellCompiler.compile( String, File ).</p>
  * 
  * @author Leif Frenzel
  */
public interface ICompilerOutput {

  /** <p>returns the exit status given by the compiler.</p> */
  int getExitStatus();
  /** <p>returns what the compiler wrote to the out stream.</p> */
  String getOutput();
  /** <p>returns what the compiler wrote to the error stream.</p> */
  Collection<ICompilerOutputItem> getErrors();
  /** <p>returns exceptions that occured in the compiler wrapper.</p> */
  List getExceptions();

}