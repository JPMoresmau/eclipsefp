package net.sf.eclipsefp.haskell.ui.actions;

import java.util.Collection;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.wizards.cabal.CabalTestWizard;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.osgi.service.resolver.VersionRange;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.osgi.framework.Version;

/**
 * <p>Cabal test action, contextual on project</p>
  *
  * @author JP Moresmau
 */
public class CabalTestAction implements IObjectActionDelegate {
  private Shell currentShell;
  private IProject project;

  @Override
  public void setActivePart( final IAction arg0, final IWorkbenchPart arg1 ) {
    currentShell=arg1.getSite().getShell();

  }


  @Override
  public void run( final IAction arg0 ) {
    if (project!=null){
      Version v=CabalImplementationManager.getCabalLibraryVersion();
      if (v!=null && new VersionRange(new Version(1,10,0),true,null,true).isIncluded( v )){
        WizardDialog wd=new WizardDialog( currentShell, new CabalTestWizard( project ) );
        wd.open();
      } else {
        final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),UITexts.test_version_fail);
        ErrorDialog.openError( currentShell, UITexts.test_error, UITexts.test_version_fail, st);
      }
    }
  }

  @Override
  public void selectionChanged( final IAction arg0, final ISelection arg1 ) {
    Collection<IProject> prjs=ResourceUtil.getProjects( arg1 );
    if (prjs.size()>0){
      project=prjs.iterator().next();
    }
  }

}
