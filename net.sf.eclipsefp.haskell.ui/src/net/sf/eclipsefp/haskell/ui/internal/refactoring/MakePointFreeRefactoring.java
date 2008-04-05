// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
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
