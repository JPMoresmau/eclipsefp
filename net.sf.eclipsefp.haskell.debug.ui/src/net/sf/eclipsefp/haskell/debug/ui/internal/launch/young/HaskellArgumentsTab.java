// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import java.io.File;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.young.ILaunchAttributes;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.osgi.util.NLS;
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
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;


/** <p>The arguments tab for configuring Haskell launch configurations.</p>
  *
  * @author Leif Frenzel
  * @author Alejandro Serrano
  */
public class HaskellArgumentsTab extends AbstractLaunchConfigurationTab {

  private Text txtWorkDirectory;
  private Button btnWorkingDirFile;
  private Button btnWorkingDirWorkspace;

  private Button runBackgroundButton;
  private Button syncStreamsButton;
  private Text argumentField;
  private Text stanzaField;

  private SelectionAdapter selectionAdapter;

  private final ModifyListener modifyListener = new ModifyListener() {
    public void modifyText( final ModifyEvent e ) {
      updateLaunchConfigurationDialog();
    }
  };


  // interface methods of ILaunchConfigurationTab
  ///////////////////////////////////////////////

  public void createControl( final Composite parent ) {
    Composite mainComposite = new Composite( parent, SWT.NONE );
    setControl( mainComposite );

    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    mainComposite.setLayout( layout );
    mainComposite.setLayoutData( gridData );
    mainComposite.setFont( parent.getFont() );
    createStanzaComponent( mainComposite );
    createWorkDirectoryComponent( mainComposite );
    createArgumentComponent( mainComposite );
    createVerticalSpacer( mainComposite, 2 );
    createRunBackgroundComponent( mainComposite );
    createSyncStreamsComponent( mainComposite );
  }

  public void setDefaults( final ILaunchConfigurationWorkingCopy configWc ) {
    configWc.setAttribute( ILaunchAttributes.RUN_IN_BACKGROUND, true );
    configWc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, ILaunchAttributes.EMPTY );
    configWc.setAttribute( ILaunchAttributes.EXTRA_ARGUMENTS, ILaunchAttributes.EMPTY );
    configWc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
    configWc.setAttribute( ILaunchAttributes.STANZA, ILaunchAttributes.EMPTY );
  }

  public void initializeFrom( final ILaunchConfiguration configuration ) {
    updateStanza ( configuration );
    updateWorkingDirectory( configuration );
    updateArgument( configuration );
    updateRunBackground( configuration );
    updateSyncStreams ( configuration );
  }

  public void performApply( final ILaunchConfigurationWorkingCopy configWc ) {
    String workingDirectory = txtWorkDirectory.getText().trim();
    if( workingDirectory.length() == 0 ) {
      configWc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY,
                             ( String )null );
    } else {
      configWc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY,
                             workingDirectory );
    }

    setAttribute( ILaunchAttributes.RUN_IN_BACKGROUND,
                  configWc,
                  runBackgroundButton.getSelection(),
                  true );

    setAttribute( ILaunchAttributes.SYNC_STREAMS,
        configWc,
        syncStreamsButton.getSelection(),
        true );

    String arguments = argumentField.getText().trim();
    if( arguments.length() == 0 ) {
      configWc.setAttribute( ILaunchAttributes.EXTRA_ARGUMENTS, ( String )null );
    } else {
      configWc.setAttribute( ILaunchAttributes.EXTRA_ARGUMENTS, arguments );
    }

    String stanza = stanzaField.getText().trim();
    if( stanza.length() == 0 ) {
      configWc.setAttribute( ILaunchAttributes.STANZA, ( String )null );
    } else {
      configWc.setAttribute( ILaunchAttributes.STANZA, stanza );
    }
  }

  @Override
  public boolean isValid( final ILaunchConfiguration launchConfig ) {
    setErrorMessage( null );
    setMessage( null );
    return validateWorkDirectory() && validateStanza();
  }

  public String getName() {
    return UITexts.haskellArgumentsTab_name;
  }

  @Override
  public Image getImage() {
    return HaskellUIImages.getImage( IImageNames.LAUNCH_TAB_ARGUMENTS );
  }

  @Override
  public boolean canSave() {
    return isValid( null );
  }


  // UI creation methods
  //////////////////////

  private void createWorkDirectoryComponent( final Composite parent ) {
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
    label.setText( UITexts.haskellArgumentsTab_workDir );
    label.setFont( font );

    txtWorkDirectory = new Text( composite, SWT.BORDER );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = IDialogConstants.ENTRY_FIELD_WIDTH;
    txtWorkDirectory.setLayoutData( data );
    txtWorkDirectory.setFont( font );

    createButtonComposite( parent, font );
  }

  private void createButtonComposite( final Composite parent,
                                      final Font font ) {
    Composite buttonComposite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.marginWidth = 0;
    layout.marginHeight = 0;
    layout.numColumns = 1;

    GridData gridData = new GridData( GridData.HORIZONTAL_ALIGN_END );
    buttonComposite.setLayout( layout );
    buttonComposite.setLayoutData( gridData );
    buttonComposite.setFont( font );

    createVerticalSpacer( buttonComposite, 1 );
    String browseWS = UITexts.haskellArgumentsTab_browseWs;
    btnWorkingDirWorkspace
      = createPushButton( buttonComposite, browseWS, null );
    btnWorkingDirWorkspace.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        handleWorkspaceWorkingDirectoryButtonSelected();
      }
    } );
    String browseFs = UITexts.haskellArgumentsTab_browseFs;
    btnWorkingDirFile = createPushButton( buttonComposite, browseFs, null );
    btnWorkingDirFile.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        handleFileWorkingDirectoryButtonSelected();
      }
    } );
  }

  private void createStanzaComponent( final Composite parent ) {
    Font font = parent.getFont();

    Composite composite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.marginWidth = 0;
    layout.marginHeight = 0;
    layout.numColumns = 2;
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalSpan = 2;
    composite.setLayout( layout );
    composite.setLayoutData( gridData );

    Label label = new Label( composite, SWT.NONE );
    label.setText( UITexts.haskellArgumentsTab_lblStanza );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    label.setLayoutData( data );
    label.setFont( font );

    stanzaField = new Text( composite, SWT.BORDER );
    GridData stanzaData = new GridData( GridData.FILL_HORIZONTAL );
    stanzaField.setLayoutData( stanzaData );
    stanzaField.setFont( font );
    stanzaField.addModifyListener( modifyListener );
  }

  private void createArgumentComponent( final Composite parent ) {
    Font font = parent.getFont();

    Label label = new Label( parent, SWT.NONE );
    label.setText( UITexts.haskellArgumentsTab_lblArguments );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.horizontalSpan = 2;
    label.setLayoutData( data );
    label.setFont( font );

    int style = SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.V_SCROLL;
    argumentField = new Text( parent, style );
    data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = IDialogConstants.ENTRY_FIELD_WIDTH;
    data.heightHint = 40;
    data.horizontalSpan = 2;
    argumentField.setLayoutData( data );
    argumentField.setFont( font );
    argumentField.addModifyListener( modifyListener );

    Label noteQuote = new Label( parent, SWT.NONE );
    noteQuote.setText( UITexts.haskellArgumentsTab_noteQuote );
    GridData noteQuoteData = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    noteQuoteData.horizontalSpan = 2;
    label.setLayoutData( noteQuoteData );
    label.setFont( font );
  }

  private void createRunBackgroundComponent( final Composite parent ) {
    runBackgroundButton = new Button( parent, SWT.CHECK );
    runBackgroundButton.setText( UITexts.haskellArgumentsTab_background );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.horizontalSpan = 2;
    runBackgroundButton.setLayoutData( data );
    runBackgroundButton.setFont( parent.getFont() );
    runBackgroundButton.addSelectionListener( getSelectionAdapter() );
  }

  private void createSyncStreamsComponent( final Composite parent ) {
    syncStreamsButton = new Button( parent, SWT.CHECK );
    syncStreamsButton.setText( UITexts.haskellArgumentsTab_syncStreams );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.horizontalSpan = 2;
    syncStreamsButton.setLayoutData( data );
    syncStreamsButton.setFont( parent.getFont() );
    syncStreamsButton.addSelectionListener( getSelectionAdapter() );
  }

  // helping methods
  //////////////////

  private void updateWorkingDirectory( final ILaunchConfiguration config ) {
    String workingDir= ILaunchAttributes.EMPTY;
    try {
      workingDir = config.getAttribute( ILaunchAttributes.WORKING_DIRECTORY,
                                        ILaunchAttributes.EMPTY );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( UITexts.error_read_configuration, ce );
    }
    txtWorkDirectory.setText( workingDir );
    txtWorkDirectory.addModifyListener( modifyListener );
  }

  private void updateArgument( final ILaunchConfiguration configuration ) {
    String arguments= ILaunchAttributes.EMPTY;
    try {
      String att = ILaunchAttributes.EXTRA_ARGUMENTS;
      arguments = configuration.getAttribute( att, ILaunchAttributes.EMPTY );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( UITexts.error_read_configuration, ce );
    }
    argumentField.setText( arguments );
  }

  private void updateRunBackground( final ILaunchConfiguration config ) {
    boolean runInBackground = true;
    try {
      runInBackground
        = config.getAttribute( ILaunchAttributes.RUN_IN_BACKGROUND, true );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( UITexts.error_read_configuration, ce );
    }
    runBackgroundButton.setSelection( runInBackground );
  }

  private void updateSyncStreams( final ILaunchConfiguration config ) {
    boolean syncStreams = true;
    try {
      syncStreams
        = config.getAttribute( ILaunchAttributes.SYNC_STREAMS, true );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( UITexts.error_read_configuration, ce );
    }
    syncStreamsButton.setSelection( syncStreams );
  }

  private void updateStanza( final ILaunchConfiguration configuration ) {
    String stanza = ILaunchAttributes.EMPTY;
    try {
      String att = ILaunchAttributes.STANZA;
      stanza = configuration.getAttribute( att, ILaunchAttributes.EMPTY );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( UITexts.error_read_configuration, ce );
    }
    stanzaField.setText( stanza );
  }

  /** validates the content of the working directory field. */
  private boolean validateWorkDirectory() {
    boolean result = true;
    String value = txtWorkDirectory.getText().trim();
    if( value.length() > 0 ) {
      File file = new File( value );
      if( !file.exists() ) {
        setErrorMessage( UITexts.haskellArgumentsTab_noWorkDir );
        result = false;
      } else if( !file.isDirectory() ) {
        String msg = UITexts.haskellArgumentsTab_noDir;
        setErrorMessage( NLS.bind( msg, file.toString() ) );
        result = false;
      }
    }
    return result;
  }

  /** validates the content of the working directory field. */
  private boolean validateStanza() {
    boolean result = true;
    String value = stanzaField.getText().trim();
    if (value.length() == 0) {
      setErrorMessage( UITexts.haskellArgumentsTab_noStanza );
      result = false;
    }
    return result;
  }

  /** prompts the user for a working directory location within the
    * workspace and sets the working directory. */
  private void handleWorkspaceWorkingDirectoryButtonSelected() {
    ContainerSelectionDialog containerDialog;
    IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
    String msg = UITexts.haskellArgumentsTab_selectDir;
    containerDialog
      = new ContainerSelectionDialog( getShell(), wsRoot, false, msg );
    containerDialog.open();
    Object[] resource = containerDialog.getResult();
    String text = null;
    if( resource != null && resource.length > 0 ) {
      IPath path = ( IPath )resource[ 0 ];
      text = wsRoot.getLocation().append( path ).toOSString();
    }
    if( text != null ) {
      txtWorkDirectory.setText( text );
    }
  }

  private void handleFileWorkingDirectoryButtonSelected() {
    DirectoryDialog dialog = new DirectoryDialog( getShell(), SWT.SAVE );
    dialog.setMessage( UITexts.haskellArgumentsTab_selectDir );
    dialog.setFilterPath( txtWorkDirectory.getText() );
    String text = dialog.open();
    if( text != null ) {
      txtWorkDirectory.setText( text );
    }
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