// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.ui;

import net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings.*;
import net.sf.eclipsefp.haskell.refactoring.internal.ui.wizards.RenameModuleWizard;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/** <p>The main plugin class for the Refactoring UI.</p>
  * 
  * @author The mighty PDE wizard
  */
public class HaskellRefactoringUI extends AbstractUIPlugin {
  
  private static HaskellRefactoringUI plugin;

  public HaskellRefactoringUI() {
    super();
    plugin = this;
  }

  public static HaskellRefactoringUI getDefault() {
    return plugin;
  }
  
  
  // refactoring entry points
  ///////////////////////////
  
  public void rename( final Shell shell, final IRenameModuleInfo info ) {
    if( saveAll() ) {
      RenameModuleProcessor processor = new RenameModuleProcessor( info );
      RenameModuleRefactoring ref = new RenameModuleRefactoring( processor );
      RefactoringWizardOpenOperation op 
        = new RefactoringWizardOpenOperation( new RenameModuleWizard( ref ) );
      try {
        op.run( shell, "" );
      } catch( InterruptedException irex ) {
        // operation was cancelled
      }
    }
  }

  
  // logging and tracing
  //////////////////////

  public static void log( final String msg, final Throwable thr ) {
    String pluginId = getDefault().getBundle().getSymbolicName();
    IStatus status = new Status( IStatus.ERROR, pluginId, 0, msg, thr );
    getDefault().getLog().log( status );
  }
  
  
  // helping methods
  //////////////////

  private static boolean saveAll() {
    IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
    return IDE.saveAllEditors( new IResource[] { workspaceRoot }, false );
  }
}
