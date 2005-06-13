// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.ui.wizards;

import net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings.RenameModuleRefactoring;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;

/** <p>The wizard that is shown to the user for entering the necessary 
  * information for module renaming.</p>
  * 
  * @author Leif Frenzel
  */
public class RenameModuleWizard extends RefactoringWizard {

  public RenameModuleWizard( final RenameModuleRefactoring refactoring ) {
    super( refactoring, DIALOG_BASED_USER_INTERFACE );
  }
  

  // interface methods of RefactoringWizard
  /////////////////////////////////////////
  
  protected void addUserInputPages() {
    setDefaultPageTitle( getRefactoring().getName() );
    addPage( new RenameModuleInputPage() );
  }
}
