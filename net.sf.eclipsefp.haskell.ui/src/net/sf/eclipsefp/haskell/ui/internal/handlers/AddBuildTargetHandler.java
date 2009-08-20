package net.sf.eclipsefp.haskell.ui.internal.handlers;

import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.ui.internal.wizards.NewBuildTargetWizard;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;


public class AddBuildTargetHandler extends AbstractHandler implements IHandler {

  public Object execute( final ExecutionEvent event ) {
    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

    IHaskellProject project = getCurrentProject();
    if (project == null) {
      MessageDialog.openInformation( shell, "No Haskell project selected", "Haskell build targets can only be added to Haskell projects. Select an item inside a Haskell project, then try again." );
    } else {
      NewBuildTargetWizard wizard = new NewBuildTargetWizard(project);
      WizardDialog dialog = new WizardDialog( shell, wizard );
      dialog.open();
    }

    return null; // must return null
  }

  private IHaskellProject getCurrentProject() {
    ISelection selection = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getSelectionService().getSelection();
    if (selection instanceof ITreeSelection) {
      ITreeSelection treeSelection = (ITreeSelection)selection;
      TreePath[] paths = treeSelection.getPaths();
      for (TreePath path : paths) {
        for (int i = 0; i < path.getSegmentCount(); ++i) {
          if (path.getSegment( i ) instanceof IProject) {
            IProject project = (IProject)path.getSegment( i );
            try {
              if (project.isAccessible() && project.hasNature( HaskellNature.NATURE_ID )) {
                IHaskellProject hsProject = HaskellProjectManager.get( project );
                if (hsProject != null) {
                  return hsProject;
                }
              }
            } catch( CoreException ex ) {
              // ignore and continue
            }
          }
        }
      }
    }
    return null;
  }

}
