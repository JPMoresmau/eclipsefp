// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring.wizards;

import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;


/** <p>The wizard that is shown to the user during the Rename refactoring.</p>
  *
  * <p>The wizard class is primarily needed for deciding which pages are
  * shown to the user. The actual user interface creation goes on the
  * pages.</p>
  *
  * @author Leif Frenzel
  */
public class RenameWizard extends RefactoringWizard {

  public RenameWizard( final Refactoring refactoring ) {
    super( refactoring, DIALOG_BASED_USER_INTERFACE );
  }


  // interface methods of RefactoringWizard
  /////////////////////////////////////////

  @Override
  protected void addUserInputPages() {
    // TODO lf page for new name
    // no particular pages - just have the usual preview page
    setDefaultPageTitle( getRefactoring().getName() );
  }
}
