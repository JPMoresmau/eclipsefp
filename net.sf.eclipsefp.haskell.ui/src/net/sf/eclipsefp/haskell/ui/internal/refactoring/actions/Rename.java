// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring.actions;

import net.sf.eclipsefp.haskell.ui.internal.refactoring.Ref;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.RefInfo;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.RefProcessor;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.RenameDelegate;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.wizards.RenameWizard;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.ui.IEditorActionDelegate;

/** <p>an action for triggering the Rename refactoring on a text selection.</p>
  *
  * <p>This action is declared in the <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class Rename extends RefAction implements IEditorActionDelegate {


  @Override
  void openWizard() {
    RefactoringProcessor processor = new RenameProcessor( info );
    RenameWizard wizard = new RenameWizard( new Ref( processor ) );
    RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation( wizard );
    try {
      String titleForFailedChecks = ""; //$NON-NLS-1$
      op.run( getShell(), titleForFailedChecks );
    } catch( final InterruptedException irex ) {
      // operation was cancelled
    }
  }

  private class RenameProcessor extends RefProcessor {
    public RenameProcessor( final RefInfo info ) {
      super( new RenameDelegate( info ), UITexts.renameProcessor_name );
    }
  }
}
