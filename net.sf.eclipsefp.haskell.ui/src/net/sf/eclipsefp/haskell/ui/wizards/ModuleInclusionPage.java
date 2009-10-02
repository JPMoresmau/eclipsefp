package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.properties.ModuleInclusionComposite;
import org.eclipse.core.resources.IContainer;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>Give options to expose and include modules in Cabal sections</p>
  *
  * @author JP Moresmau
 */
public class ModuleInclusionPage extends StatusWizardPage{
  ModuleInclusionComposite mic;

  public ModuleInclusionPage() {
    super( "ModuleInclusionPage" ); //$NON-NLS-1$
    setTitle( UITexts.module_inclusion_page_title );
    setDescription( UITexts.module_inclusion_page_description );
  }

  public void createControl( final Composite parent ) {
    initializeDialogUnits( parent );

    mic=new ModuleInclusionComposite( parent, SWT.NONE );
    setControl( mic );

    Dialog.applyDialogFont( mic );
  }


  public ModuleInclusionComposite getModuleInclusionComposite() {
    return mic;
  }

  @Override
  public void setPreviousPage( final IWizardPage page ) {
    super.setPreviousPage( page );
    ModuleCreationInfo mci=((NewModuleWizardPage)page).getInfo();
    IContainer src=mci.getSourceContainer();
    String module=mci.getQualifiedModuleName();
    mic.init( src, module );

  }
}
