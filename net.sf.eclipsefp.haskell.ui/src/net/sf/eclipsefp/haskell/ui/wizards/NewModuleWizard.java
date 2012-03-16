// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;


/**
 * <p>
 * The wizard for creating a new Haskell module.
 * </p>
 *
 * @author Leif Frenzel
 */
public class NewModuleWizard extends RevealAtEndWizard implements INewWizard {

  public static final String ID = NewModuleWizard.class.getName();

  private IStructuredSelection selection;
  private NewModuleWizardPage page0;
  private ModuleInclusionPage page1;

  public NewModuleWizard() {
    super();
    setNeedsProgressMonitor( true );
    setWindowTitle( UITexts.NewModuleWizard_0 );
    initBannerImage();
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
  }

  @Override
  public void init( final IWorkbench workbench,
      final IStructuredSelection selection ) {
    this.selection = selection;
  }

  @Override
  public void addPages() {
    super.addPages();
    page0 = new NewModuleWizardPage();
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
    ModuleCreationOperation mco = new ModuleCreationOperation( mci );

    IRunnableWithProgress op = new WorkspaceModifyDelegatingOperation( mco );
    boolean result = false;
    try {
      getContainer().run( false, true, op );
      result = true;
      finish( mco.getGeneratedFile() );
    } catch( Exception ex ) {
      handleFinishException( ex );
      HaskellUIPlugin.log( UITexts.NewModuleWizard_1, ex );
    }
    return result;
  }


  // helping methods
  // ////////////////

  private void handleFinishException( final Exception ex ) {
    String msg = NLS.bind( UITexts.NewModuleWizard_2 , ex.getLocalizedMessage());
    MessageDialog.openError( getShell(), UITexts.NewModuleWizard_4, msg );
  }

  private void finish( final IFile createdFile ) {
    if( createdFile != null ) {
      selectAndReveal( createdFile );
      openResource( createdFile );
    }
  }

  private void initBannerImage() {
    String key = IImageNames.NEW_MODULE;
    ImageDescriptor imageDescriptor = HaskellUIImages.getImageDescriptor( key );
    setDefaultPageImageDescriptor( imageDescriptor );
  }
}