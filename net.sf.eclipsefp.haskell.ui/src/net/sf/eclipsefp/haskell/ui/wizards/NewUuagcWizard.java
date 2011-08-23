package net.sf.eclipsefp.haskell.ui.wizards;

import java.io.IOException;
import java.io.InputStream;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;


public class NewUuagcWizard extends RevealAtEndWizard implements INewWizard {

  public static final String ID = NewUuagcWizard.class.getName();
  static final String FILE_NAME = "/newFiles/uuagc.ag";
  static final String FILE_NAME_HASKELL = "/newFiles/uuagc-haskell.ag";

  private IStructuredSelection selection;
  private NewUuagcWizardPage page0;
  private ModuleInclusionPage page1;

  public NewUuagcWizard() {
    super();
    setNeedsProgressMonitor( true );
    setWindowTitle( UITexts.new_uuagc );
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
  }

  public void init( final IWorkbench workbench,
      final IStructuredSelection selection ) {
    this.selection = selection;
  }

  @Override
  public void addPages() {
    super.addPages();
    page0 = new NewUuagcWizardPage( "NewUuagcPage", UITexts.new_uuagc,
        UITexts.uuagc_newFile );
    addPage( page0 );
    page0.init( selection );
    page1 = new ModuleInclusionPage();
    addPage( page1 );
  }

  @Override
  public boolean performFinish() {
    ModuleCreationInfo mci = page0.getInfo();
    // the user hasn't clicked on next
    if( !page1.getModuleInclusionComposite().isInit() ) {
      page1.setPreviousPage( page0 );
    }
    mci.setExposed( page1.getModuleInclusionComposite().getExposed() );
    mci.setIncluded( page1.getModuleInclusionComposite().getIncluded() );
    UuagcFileCreationOperator mco = new UuagcFileCreationOperator( mci,
        getInitialContents( page0.getUseHaskellSyntax() ), "ag",
        page0.getUseHaskellSyntax() );

    IRunnableWithProgress op = new WorkspaceModifyDelegatingOperation( mco );
    boolean result = false;
    try {
      getContainer().run( false, true, op );
      result = true;
      finish( mco.getGeneratedFile() );
    } catch( Exception ex ) {
      handleFinishException( ex );
      HaskellUIPlugin.log( "Error creating new module.", ex );
    }
    return result;
  }

  // helping methods
  // ////////////////

  private void handleFinishException( final Exception ex ) {
    String msg = "The following error occured: " + ex.getLocalizedMessage()
        + "Please see workspace/.metadata/.log for more information.";
    MessageDialog.openError( getShell(), "Problem occured", msg );
  }

  private void finish( final IFile createdFile ) {
    if( createdFile != null ) {
      selectAndReveal( createdFile );
      openResource( createdFile );
    }
  }

  protected InputStream getInitialContents( final boolean useHaskellSyntax ) {
    try {
      return HaskellUIPlugin.getDefault().getBundle()
          .getEntry( useHaskellSyntax ? FILE_NAME_HASKELL : FILE_NAME )
          .openStream();
    } catch( IOException ex ) {
      // This should never happen
      return null;
    }
  }

}
