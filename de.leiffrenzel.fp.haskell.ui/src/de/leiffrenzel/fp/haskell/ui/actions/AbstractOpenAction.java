// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;

import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;
import de.leiffrenzel.fp.haskell.ui.util.PixelConverter;


/** <p>Superclass for actions that open some workbench elements.</p>
  * 
  * @author Leif Frenzel
  */
abstract class AbstractOpenAction extends Action {
  
  // common functionality for subclasses
  //////////////////////////////////////
  
  void handleError( final Exception ex, final String msg ) {
    IStatus status = new Status( IStatus.ERROR,
                                 HaskellUIPlugin.getPluginId(),
                                 IStatus.ERROR, 
                                 msg, 
                                 ex );
    HaskellUIPlugin.log( msg, ex );
    ErrorDialog.openError( getActiveWBWindowShell(), "Error", msg, status );
  }

  IWorkbench getWorkbench() {
    return HaskellUIPlugin.getDefault().getWorkbench();
  }
  
  Shell getActiveWBWindowShell() {
    return getWorkbench().getActiveWorkbenchWindow().getShell();
  }
  
  WizardDialog createDialog( final Wizard wizard ) {
    Shell activeWBWindowShell = getActiveWBWindowShell();
    WizardDialog dialog = new WizardDialog( activeWBWindowShell, wizard );
    PixelConverter converter = new PixelConverter( activeWBWindowShell );
      
    dialog.setMinimumPageSize( converter.convertWidthInCharsToPixels( 70 ), 
                               converter.convertHeightInCharsToPixels( 20 ) );
    return dialog;
  }
}