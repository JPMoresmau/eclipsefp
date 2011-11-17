package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import net.sf.eclipsefp.haskell.debug.core.internal.launch.young.ILaunchAttributes;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 *
 * @author JP Moresmau
 *
 */
public class HaskellAutomationTab extends AbstractLaunchConfigurationTab {

  private Text txtCommand;
  private Button btnReloadSave;
  private Button btnReloadCommand;

  private SelectionAdapter selectionAdapter;

  private final ModifyListener modifyListener = new ModifyListener() {
    public void modifyText( final ModifyEvent e ) {
      updateLaunchConfigurationDialog();
    }
  };

  public void createControl( final Composite parent ) {
    Composite mainComposite = new Composite( parent, SWT.NONE );
    setControl( mainComposite );

    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    mainComposite.setLayout( layout );
    mainComposite.setLayoutData( gridData );
    mainComposite.setFont( parent.getFont() );

    createCommand(mainComposite);
    createReload(mainComposite);
    createReloadCommand(mainComposite);
  }




  public String getName() {
    return UITexts.haskellAutomationTab_name;
  }

  @Override
  public Image getImage() {
    return HaskellUIImages.getImage( IImageNames.LAUNCH_TAB_AUTOMATION );
  }


  public void initializeFrom( final ILaunchConfiguration configuration ) {
    updateCommand( configuration );
    updateReloadSave( configuration );
    updateReloadCommand( configuration );

  }

  public void performApply( final ILaunchConfigurationWorkingCopy configWc ) {
    String command = txtCommand.getText().trim();
    if( command.length() == 0 ) {
      configWc.setAttribute( ILaunchAttributes.COMMAND,(String)null );
    } else {
      configWc.setAttribute( ILaunchAttributes.COMMAND, command );
    }

    setAttribute( ILaunchAttributes.RELOAD, configWc,
        btnReloadSave.getSelection(), false );

    setAttribute( ILaunchAttributes.COMMAND_ON_RELOAD, configWc, btnReloadCommand
        .getSelection(), false );
  }

  public void setDefaults( final ILaunchConfigurationWorkingCopy configuration ) {
    configuration.setAttribute( ILaunchAttributes.COMMAND,  (String)null );
    configuration.setAttribute( ILaunchAttributes.RELOAD, false );
    configuration.setAttribute( ILaunchAttributes.COMMAND_ON_RELOAD, false );
  }

  @Override
  protected void updateLaunchConfigurationDialog() {
    btnReloadCommand.setEnabled( txtCommand.getText().trim().length()>0 && btnReloadSave.getSelection() );
    super.updateLaunchConfigurationDialog();
  }

  private void createCommand( final Composite parent ) {
    Font font = parent.getFont();

    Composite composite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.marginWidth = 0;
    layout.marginHeight = 0;
    layout.numColumns = 1;
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    composite.setLayout( layout );
    composite.setLayoutData( gridData );

    Label label = new Label( composite, SWT.NONE );
    label.setText( UITexts.haskellAutomationTab_command );
    label.setFont( font );

    txtCommand = new Text( composite, SWT.BORDER );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = IDialogConstants.ENTRY_FIELD_WIDTH;
    txtCommand.setLayoutData( data );
    txtCommand.setFont( font );
    txtCommand.addModifyListener( modifyListener );
  }

  private void createReload( final Composite parent) {
    btnReloadSave = new Button( parent, SWT.CHECK );
    btnReloadSave.setText( UITexts.haskellAutomationTab_reload );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.horizontalSpan = 2;
    btnReloadSave.setLayoutData( data );
    btnReloadSave.setFont( parent.getFont() );
    btnReloadSave.addSelectionListener( getSelectionAdapter() );
  }

  private void createReloadCommand( final Composite parent) {
    btnReloadCommand = new Button( parent, SWT.CHECK );
    btnReloadCommand.setText( UITexts.haskellAutomationTab_command_reload );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.horizontalSpan = 2;
    btnReloadCommand.setLayoutData( data );
    btnReloadCommand.setFont( parent.getFont() );
    btnReloadCommand.addSelectionListener( getSelectionAdapter() );
  }

  private void updateCommand( final ILaunchConfiguration configuration){
    String command= ILaunchAttributes.EMPTY;
    try {
      String att = ILaunchAttributes.COMMAND;
      command = configuration.getAttribute( att, ILaunchAttributes.EMPTY );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( UITexts.error_read_configuration, ce );
    }
    txtCommand.setText( command );
  }

  private void updateReloadSave( final ILaunchConfiguration config ) {
    boolean reload = true;
    try {
      reload
        = config.getAttribute( ILaunchAttributes.RELOAD, false );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( UITexts.error_read_configuration, ce );
    }
    btnReloadSave.setSelection( reload );
  }

  private void updateReloadCommand( final ILaunchConfiguration config ) {
    boolean reload = true;
    try {
      reload
        = config.getAttribute( ILaunchAttributes.COMMAND_ON_RELOAD, false );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( UITexts.error_read_configuration, ce );
    }
    btnReloadCommand.setSelection( reload );
  }

  private SelectionListener getSelectionAdapter() {
    if( selectionAdapter == null ) {
      selectionAdapter= new SelectionAdapter() {
        @Override
        public void widgetSelected( final SelectionEvent e ) {
          updateLaunchConfigurationDialog();
        }
      };
    }
    return selectionAdapter;
  }
}
