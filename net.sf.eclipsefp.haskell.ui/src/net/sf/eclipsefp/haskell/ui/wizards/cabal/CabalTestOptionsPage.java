package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.util.Collections;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>Options page for cabal test</p>
  *
  * @author JP Moresmau
 */
public class CabalTestOptionsPage extends WizardPage {
  private DistFolder dFolder;
  private final IProject project;

  public CabalTestOptionsPage( final IProject project ) {
    super( "TestOptions",UITexts.test_options,null );
    this.project=project;
  }

  public void createControl( final Composite parent ) {
    initializeDialogUnits( parent );
    Composite composite = new Composite( parent, SWT.NONE );
    GridData gd=new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
    composite.setLayoutData( gd );
    int cols=3;
    GridLayout layout=new GridLayout( cols, false );
    composite.setLayout( layout );

    dFolder=new DistFolder(Collections.singleton( project ),composite, UITexts.test_options_folder,UITexts.test_options_folder_choose,UITexts.test_options_folder_choose );

    setControl( composite );
    Dialog.applyDialogFont( composite );
  }


  public String getFolder(){
    return dFolder.getFolder();
  }
}
