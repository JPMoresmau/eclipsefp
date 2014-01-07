// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring.actions;

import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageThread;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.RenameDelegate;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.wizards.RenameWizard;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.ui.IEditorActionDelegate;

/** <p>an action for triggering the Rename refactoring on a text selection.</p>
  *
  * <p>This action is declared in the <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class Rename extends RefAction implements IEditorActionDelegate {

  /* (non-Javadoc)
   * @see org.eclipse.core.commands.AbstractHandler#isEnabled()
   */
  @Override
  public boolean isEnabled() {
    // cannot rename if we're still analyzing usage
    UsageThread ut=BuildWrapperPlugin.getDefault().getUsageThread();
    if (ut!=null && ut.isWorking()){
      return false;
    }
    return super.isEnabled();
  }

  @Override
  synchronized void openWizard() {

    RenameWizard wizard = new RenameWizard( new RenameDelegate( info ));
    RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation( wizard );
    try {
      String titleForFailedChecks = ""; //$NON-NLS-1$
      int code=op.run( getShell(), titleForFailedChecks );
      if (IDialogConstants.OK_ID==code){
        if (info.getSourceFile()!=null && BuildWrapperPlugin.getDefault()!=null){
          UsageThread ut=BuildWrapperPlugin.getDefault().getUsageThread();
          if (ut!=null){
            ut.addProject(info.getSourceFile().getProject());
          }
        }
      }
    } catch( final InterruptedException irex ) {
      // operation was cancelled
    }
  }


}
