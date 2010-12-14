package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.util.Collection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>Options page for cabal install</p>
  *
  * @author JP Moresmau
 */
public class CabalInstallOptionsPage extends WizardPage {
  private DistFolder dFolder;
  private final Collection<IProject> projects;
  private boolean global=false;

  public CabalInstallOptionsPage(final Collection<IProject> projects) {
    super( "InstallOptions", UITexts.install_options, null );
    this.projects=projects;

  }

  public void createControl( final Composite parent ) {
    initializeDialogUnits( parent );
    Composite composite = new Composite( parent, SWT.NONE );
    GridData gd=new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
    composite.setLayoutData( gd );
    int cols=projects.size()==1?3:2;
    GridLayout layout=new GridLayout( cols, false );
    composite.setLayout( layout );

    dFolder=new DistFolder(projects,composite, UITexts.install_options_folder,UITexts.install_options_folder_choose,UITexts.install_options_folder_choose );

    final Button bUser=new Button(composite,SWT.RADIO);
    bUser.setText( UITexts.install_options_user );
    bUser.setSelection( true );
    bUser.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
       global=!bUser.getSelection();
      }
    });
    gd=new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
    gd.horizontalSpan=3;
    bUser.setLayoutData( gd );

    final Button bGlobal=new Button(composite,SWT.RADIO);
    bGlobal.setText( UITexts.install_options_global );
    bGlobal.setSelection( false );
    bGlobal.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
       global=bGlobal.getSelection();
      }
    });
    gd=new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
    gd.horizontalSpan=3;
    bGlobal.setLayoutData( gd );

    setControl( composite );
    Dialog.applyDialogFont( composite );

  }

  public String getFolder(){
    return dFolder.getFolder();
  }


  public boolean isGlobal() {
    return global;
  }
}
