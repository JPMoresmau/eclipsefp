// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.compiler.defaultcompiler;

import java.util.ArrayList;
import java.util.List;

import de.leiffrenzel.fp.haskell.core.compiler.ICompilerOutput;


/** <p>Default implementation for a practically empty compiler output.</p>
  *
  * @author Leif Frenzel 
  */
class DefaultHaskellCompilerOutput implements ICompilerOutput {
  
  String name;

  DefaultHaskellCompilerOutput( final String name ) {
    this.name = name;
  }
  
  public String getErrors() {
    return "";
  }

  public List getExceptions() {
    return new ArrayList( 0 );
  }

  public int getExitStatus() {
    return 0;
  }

  public String getOutput() {
    return "I didn't compile: " + name;
  }
}