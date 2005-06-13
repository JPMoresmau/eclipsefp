// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.actions;

import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWizard;

import de.leiffrenzel.fp.haskell.ui.wizards.NewHaskellProjectWizard;


/** <p>An Action for opening the New Haskell project wizard..</p>
  * 
  * @author Leif Frenzel
  */
public class OpenNewProjectWizard extends AbstractOpenAction {

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