package net.sf.eclipsefp.haskell.ui.internal.wizards;

import net.sf.eclipsefp.haskell.core.internal.project.ExecutableBuildTarget;
import net.sf.eclipsefp.haskell.core.project.IBuildTarget;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.wizards.AddBuildTargetOperation;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.PlatformUI;

/**
 * A wizard for the creation of new build targets.
 * It consists of two pages.
 * The first page has fields for the type (executable or library)
 * and the name of the resulting binary.
 * The second page depends on the selection on the first page.
 * For executables, the main function can be specified.
 * For libraries, the list of modules to be included in the library can be specified.
 */
public class NewBuildTargetWizard extends Wizard {

  private final IHaskellProject hsProject;

  private final CommonBuildTargetWizardPage commonPage;
  private final ExecutableBuildTargetWizardPage executablePage;
  private final LibraryBuildTargetWizardPage libraryPage;

  public NewBuildTargetWizard( final IHaskellProject project ) {
    super();
    this.hsProject = project;
    commonPage = new CommonBuildTargetWizardPage( );
    executablePage = new ExecutableBuildTargetWizardPage();
    libraryPage = new LibraryBuildTargetWizardPage();
    commonPage.setProject( project );
    executablePage.setProject( project );
    libraryPage.setProject( project );
  }

  @Override
  public void addPages() {
    addPage( commonPage );
    addPage( executablePage );
    addPage( libraryPage );
  }

  @Override
  public IWizardPage getNextPage(final IWizardPage currentPage) {
    if (currentPage == commonPage) {
      if (commonPage.isExecutable()) {
        return executablePage;
      } else if (commonPage.isLibrary()) {
        return libraryPage;
      }
    }
    return null;
  }

  @Override
  public IWizardPage getPreviousPage(final IWizardPage currentPage) {
    if (currentPage == executablePage || currentPage == libraryPage) {
      return commonPage;
    }
    return null;
  }

  @Override
  public boolean performFinish() {
    IBuildTarget buildTarget;
    if (commonPage.isExecutable()) {
      buildTarget = new ExecutableBuildTarget( new Path(commonPage.getName()), executablePage.getMain() );
    } else {
      // TODO TtC implement library build targets
      buildTarget = null;
    }

    AddBuildTargetOperation operation = new AddBuildTargetOperation( hsProject, buildTarget );
    IOperationHistory operationHistory = PlatformUI.getWorkbench().getOperationSupport().getOperationHistory();
    try {
      operationHistory.execute( operation, null, null );
    } catch( ExecutionException ex ) {
      HaskellUIPlugin.log( ex );
      return false;
    }
    return true;
  }

}
