// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings;

import org.eclipse.ltk.core.refactoring.participants.ProcessorBasedRefactoring;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;

/** <p>A refactoring for renaming Haskell modules in the workspace.</p>
  *
  * @author Leif Frenzel
  */
public class RenameModuleRefactoring extends ProcessorBasedRefactoring {

  private final RefactoringProcessor processor;


  public RenameModuleRefactoring( final RefactoringProcessor processor ) {
    super( processor );
    this.processor = processor;
  }

  
  // interface methods of ProcessorBasedRefactoring
  /////////////////////////////////////////////////
  
  public RefactoringProcessor getProcessor() {
    return processor;
  }
}
