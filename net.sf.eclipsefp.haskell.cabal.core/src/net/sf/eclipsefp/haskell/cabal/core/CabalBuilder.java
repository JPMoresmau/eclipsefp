// Copyright (c) 2006-2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.core;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

public class CabalBuilder extends IncrementalProjectBuilder {

  // interface methods of IncrementalProjectBuilder
  /////////////////////////////////////////////////
  
  @Override
  protected IProject[] build( final int kind, 
                              final Map args, 
                              final IProgressMonitor monitor )
                                                          throws CoreException {
System.out.println("CabalBuilder.build()");
    return null;
  }
}
