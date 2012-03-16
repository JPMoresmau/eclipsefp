package net.sf.eclipsefp.haskell.ui.actions;

import java.util.LinkedHashSet;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.wizards.cabal.CabalInstallWizard;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 * <p>Cabal install action, contextual on projects</p>
  *
  * @author JP Moresmau
 */
public class CabalInstallAction implements IObjectActionDelegate {
  private final Set<IProject> projects=new LinkedHashSet<IProject>();
  private Shell currentShell;

  @Override
  public void setActivePart( final IAction arg0, final IWorkbenchPart arg1 ) {
    currentShell=arg1.getSite().getShell();

  }

  @Override
  public void run( final IAction arg0 ) {
   WizardDialog wd=new WizardDialog( currentShell, new CabalInstallWizard( projects ) );
   wd.open();

  }

  @Override
  public void selectionChanged( final IAction arg0, final ISelection arg1 ) {
    projects.clear();
    projects.addAll( ResourceUtil.getProjects( arg1 ) );
  }

}
