// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring.wizards;

import net.sf.eclipsefp.haskell.ui.internal.refactoring.Ref;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.RefProcessor;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.RenameDelegate;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
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
  private final RenameDelegate delegate;

  public RenameWizard( final RenameDelegate delegate ) {
   super( new Ref(new RefProcessor<RenameDelegate>(delegate , UITexts.renameProcessor_name )), DIALOG_BASED_USER_INTERFACE );
   this.delegate=delegate;

  }


  // interface methods of RefactoringWizard
  /////////////////////////////////////////

  @Override
  protected void addUserInputPages() {
    setDefaultPageTitle( getRefactoring().getName() );
    addPage( new RenamePage1( delegate ) );

  }

}
