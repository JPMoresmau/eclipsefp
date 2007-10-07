// Copyright (c) 2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.refactoring.wizards;

import net.sf.eclipsefp.haskell.core.internal.refactoring.MakePointFreeRefactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;


/** <p>The wizard that is shown to the user during the pointfree 
  * refactoring.</p>
  * 
  * <p>The wizard class is primarily needed for deciding which pages are 
  * shown to the user. The actual user interface creation goes on the 
  * pages.</p>
  * 
  * @author Leif Frenzel
  */
public class MakePointFreeWizard extends RefactoringWizard {

  public MakePointFreeWizard( final MakePointFreeRefactoring refactoring ) {
    super( refactoring, DIALOG_BASED_USER_INTERFACE );
  }


  // interface methods of RefactoringWizard
  /////////////////////////////////////////

  @Override
  protected void addUserInputPages() {
    // no particular pages - just have the usual preview page
    setDefaultPageTitle( getRefactoring().getName() );
  }
}
