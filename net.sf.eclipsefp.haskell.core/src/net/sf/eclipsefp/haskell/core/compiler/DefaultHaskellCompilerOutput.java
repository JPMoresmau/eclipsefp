// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/** <p>Default implementation for a practically empty compiler output.</p>
  *
  * @author Leif Frenzel 
  */
class DefaultHaskellCompilerOutput implements ICompilerOutput {
  
  String name;

  DefaultHaskellCompilerOutput( final String name ) {
    this.name = name;
  }
  
  public Collection<ICompilerOutputItem> getErrors() {
    return new ArrayList<ICompilerOutputItem>(0);
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