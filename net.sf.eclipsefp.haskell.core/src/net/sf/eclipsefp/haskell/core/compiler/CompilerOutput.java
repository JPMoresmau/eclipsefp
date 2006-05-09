// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.util.List;

/** <p>Default implementation for a compiler's output.</p>
  * 
  * @author Leif Frenzel
  */
public class CompilerOutput implements ICompilerOutput {

  private int exitStatus;
  private String output;
  private String errors;
  private List exceptions;

  
  public CompilerOutput( final int exitStatus, 
                         final String output,
                         final String errors,
                         final List exceptions ) {
    this.exitStatus = exitStatus;
    this.output = output;
    this.errors = errors;
    this.exceptions = exceptions;
  }

  public String toString() {
    return   "Compiler output [ " + exceptions.size() + " Exceptions ]\n"
           + output
           + "\n"
           + errors;
  }
  

  // interface methods of ICompilerOutput
  ////////////////////////////////////////////// 

  public int getExitStatus() {
    return exitStatus;
  }

  public String getOutput() {
    return output;
  }

  public String getErrors() {
    return errors;
  }

  public List getExceptions() {
    return exceptions;
  }
}