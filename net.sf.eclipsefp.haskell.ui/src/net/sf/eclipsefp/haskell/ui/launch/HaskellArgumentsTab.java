// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.launch;

import java.io.File;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;

import de.leiffrenzel.fp.haskell.core.launch.ILaunchAttributes;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;


/** <p>The arguments tab for configuring Haskell launch configurations.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellArgumentsTab extends AbstractLaunchConfigurationTab {

  private Text txtWorkDirectory;
  private Button btnWorkingDirFile;
  private Button btnWorkingDirWorkspace;

  private Button runBackgroundButton;
  private Text argumentField;

  private SelectionAdapter selectionAdapter;
  
  private ModifyListener modifyListener = new ModifyListener() {
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
    createWorkDirectoryComponent( mainComposite );
    createArgumentComponent( mainComposite );
    createVerticalSpacer( mainComposite, 2 );
    createRunBackgroundComponent( mainComposite );
  }

  public void setDefaults( final ILaunchConfigurationWorkingCopy configWc ) {
    configWc.setAttribute( ILaunchAttributes.RUN_IN_BACKGROUND, true );
    configWc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, "" );
    configWc.setAttribute( ILaunchAttributes.ARGUMENTS, "" );
  }

  public void initializeFrom( final ILaunchConfiguration configuration ) {
    updateWorkingDirectory( configuration );
    updateArgument( configuration );
    updateRunBackground( configuration );
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

    String arguments = argumentField.getText().trim();
    if( arguments.length() == 0 ) {
      configWc.setAttribute( ILaunchAttributes.ARGUMENTS, ( String )null );
    } else {
      configWc.setAttribute( ILaunchAttributes.ARGUMENTS, arguments );
    }
  }

  public boolean isValid( final ILaunchConfiguration launchConfig ) {
    setErrorMessage( null );
    setMessage( null );
    return validateWorkDirectory();
  }

  public String getName() {
    return "Arguments";
  }

  public Image getImage() {
    return HaskellUIImages.getImage( IImageNames.LAUNCH_TAB_ARGUMENTS );
  }
  
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
    label.setText( "Working Directory" );
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
    String browseWS = "Browse_Wor&kspace...";
    btnWorkingDirWorkspace 
      = createPushButton( buttonComposite, browseWS, null );
    btnWorkingDirWorkspace.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        handleWorkspaceWorkingDirectoryButtonSelected();
      }
    } );
    String browseFs = "Browse File System";
    btnWorkingDirFile = createPushButton( buttonComposite, browseFs, null );
    btnWorkingDirFile.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        handleFileWorkingDirectoryButtonSelected();
      }
    } );
  }

  private void createArgumentComponent( final Composite parent ) {
    Font font = parent.getFont();

    Label label = new Label( parent, SWT.NONE );
    label.setText( "Arguments" );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.horizontalSpan = 2;
    label.setLayoutData( data );
    label.setFont( font );

    int style = SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.V_SCROLL;
    argumentField = new Text( parent, style );
    data = new GridData( GridData.FILL_BOTH );
    data.widthHint = IDialogConstants.ENTRY_FIELD_WIDTH;
    data.heightHint = 40;
    argumentField.setLayoutData( data );
    argumentField.setFont( font );
    argumentField.addModifyListener( modifyListener );

    Label instruction = new Label( parent, SWT.NONE );
    String msg =   "Note: Enclose an argument containing spaces using double "
                 + "quotes (\").";
    instruction.setText( msg );
    data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.horizontalSpan = 2;
    instruction.setLayoutData( data );
    instruction.setFont( font );
  }

  private void createRunBackgroundComponent( final Composite parent ) {
    runBackgroundButton = new Button( parent, SWT.CHECK );
    runBackgroundButton.setText( "Run in background" );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.horizontalSpan = 2;
    runBackgroundButton.setLayoutData( data );
    runBackgroundButton.setFont( parent.getFont() );
    runBackgroundButton.addSelectionListener( getSelectionAdapter() );
  }
  
  
  // helping methods
  //////////////////
  
  private void updateWorkingDirectory( final ILaunchConfiguration config ) {
    String workingDir= "";
    try {
      workingDir = config.getAttribute( ILaunchAttributes.WORKING_DIRECTORY, 
                                        "" );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( "Error reading configuration.", ce );
    }
    txtWorkDirectory.setText( workingDir );
    txtWorkDirectory.addModifyListener( modifyListener );
  }

  private void updateArgument( final ILaunchConfiguration configuration ) {
    String arguments= "";
    try {
      arguments = configuration.getAttribute( ILaunchAttributes.ARGUMENTS, "" );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( "Error reading configuration", ce );
    }
    argumentField.setText( arguments );
  }

  private void updateRunBackground( final ILaunchConfiguration config ) {
    boolean runInBackground = true;
    try {
      runInBackground 
        = config.getAttribute( ILaunchAttributes.RUN_IN_BACKGROUND, true );
    } catch( CoreException ce ) {
      HaskellUIPlugin.log( "Error reading configuration", ce );
    }
    runBackgroundButton.setSelection( runInBackground );
  }

  /** validates the content of the working directory field. */
  private boolean validateWorkDirectory() {
    boolean result = true;
    String value = txtWorkDirectory.getText().trim();
    if( value.length() > 0 ) {
      File file = new File( value );
      if( !file.exists() ) {
        setErrorMessage( "Working directory does not exist or is invalid." );
        result = false;
      } else if( !file.isDirectory() ) {
        setErrorMessage( "Not a directory: " + file.toString() );
        result = false;
      }
    }
    return result;
  }
  
  /** prompts the user for a working directory location within the 
    * workspace and sets the working directory. */
  private void handleWorkspaceWorkingDirectoryButtonSelected() {
    ContainerSelectionDialog containerDialog;
    IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
    containerDialog = new ContainerSelectionDialog( getShell(), 
                                                    wsRoot,
                                                    false,
                                                    "&Select a directory" );
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
    dialog.setMessage( "&Select a directory" );
    dialog.setFilterPath( txtWorkDirectory.getText() );
    String text = dialog.open();
    if( text != null ) {
      txtWorkDirectory.setText( text );
    }
  }

  private SelectionListener getSelectionAdapter() {
    if( selectionAdapter == null ) {
      selectionAdapter= new SelectionAdapter() {
        public void widgetSelected( final SelectionEvent e ) {
          updateLaunchConfigurationDialog();
        }
      };
    }
    return selectionAdapter;
  } 
}