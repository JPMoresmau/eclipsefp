// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.internal.ui.actions;

import net.sf.eclipsefp.common.internal.ui.wizards.ConfiguratorWizard;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/** <p>An action for running the configurator wizard.</p>
  *
  * @author Leif Frenzel
  */
public class RunConfigurator extends Action {

  
  // interface methods of Action
  //////////////////////////////
  
  public void run() {
    IWizard wizard = new ConfiguratorWizard();
    WizardDialog dlg = new WizardDialog( getShell(), wizard );
    dlg.open();
  }
  
  
  // helping methods
  //////////////////
  
  private Shell getShell() {
    return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
  }
}
