package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;


public class NewHappyWizard extends RevealAtEndWizard implements INewWizard {

  private IStructuredSelection selection;
  private WizardNewFileCreationPage mainPage;

  public NewHappyWizard() {
    super();
  }

  public void init( final IWorkbench workbench, final IStructuredSelection selection ) {
    this.selection = selection;
    setWindowTitle(UITexts.new_happy);
  }

  @Override
  public void addPages() {
    mainPage = new NewHappyPage(selection);
    addPage(mainPage);
  }

  @Override
  public boolean performFinish() {
    IFile file = mainPage.createNewFile();
    if (file != null) {
      selectAndReveal( file );
      openResource( file );
      return true;
    } else {
      return false;
    }
  }

}
