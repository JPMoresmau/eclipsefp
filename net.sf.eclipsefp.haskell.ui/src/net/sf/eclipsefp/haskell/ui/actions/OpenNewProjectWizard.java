// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.actions;

import net.sf.eclipsefp.haskell.ui.wizards.NewHaskellProjectWizard;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWizard;


/** <p>An Action for opening the New Haskell project wizard..</p>
  *
  * @author Leif Frenzel
  */
public class OpenNewProjectWizard extends AbstractOpenAction {

  @Override
  public void run() {
    try {
      Wizard wizard = new NewHaskellProjectWizard();
      if( wizard instanceof IWorkbenchWizard ) {
        ( ( IWorkbenchWizard )wizard ).init( getWorkbench(), null );
      }

      WizardDialog dialog = createDialog( wizard );
      dialog.create();
      dialog.open();
    } catch( Exception ex ) {
      String message = "Could not open wizard.\nSee error log for details.";
      handleError( ex, message );
    }
  }
}