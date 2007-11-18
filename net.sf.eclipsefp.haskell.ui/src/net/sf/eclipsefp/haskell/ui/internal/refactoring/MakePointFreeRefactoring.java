// Copyright (c) 2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.refactoring;

import org.eclipse.ltk.core.refactoring.participants.ProcessorBasedRefactoring;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;

/** <p>Refactoring for transforming a code portion to pointfree style.</p>
  *
  * <p>All the actual work is done in the processor, so we just have to 
  * keep a reference to one here.<p>
  *
  * @author Leif Frenzel
  */
public class MakePointFreeRefactoring extends ProcessorBasedRefactoring {

  private final RefactoringProcessor processor;

  public MakePointFreeRefactoring( final RefactoringProcessor processor ) {
    super( processor );
    this.processor = processor;
  }

  
  // interface methods of ProcessorBasedRefactoring
  /////////////////////////////////////////////////
  
  @Override
  public RefactoringProcessor getProcessor() {
    return processor;
  }
}
