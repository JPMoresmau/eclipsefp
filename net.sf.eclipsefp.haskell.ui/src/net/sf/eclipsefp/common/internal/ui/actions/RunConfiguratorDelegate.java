// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.internal.ui.actions;

import net.sf.eclipsefp.common.internal.ui.wizards.ConfiguratorWizard;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.*;

/** <p>runs the configurator wizard.</p>
  *
  * @author Leif Frenzel
  */
public class RunConfiguratorDelegate implements IWorkbenchWindowActionDelegate {

  
  // interface methods of IWorkbenchWindowActionDelegate
  //////////////////////////////////////////////////////
  
  public void init( final IWorkbenchWindow window ) {
    // unused
  }

  public void run( final IAction action ) {
    IWizard wizard = new ConfiguratorWizard();
    WizardDialog dlg = new WizardDialog( getShell(), wizard );
    dlg.open();
  }

  public void selectionChanged( final IAction action, 
                                final ISelection selection ) {
    // unused
  }

  public void dispose() {
    // unused
  }
  
  
  // helping methods
  //////////////////
  
  private Shell getShell() {
    return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
  }
}
