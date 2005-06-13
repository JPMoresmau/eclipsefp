// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.wizards;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

import de.leiffrenzel.fp.haskell.ui.dialog.FolderSelectionDialog;
import de.leiffrenzel.fp.haskell.ui.dialog.SourceFolderSelectionDialog;
import de.leiffrenzel.fp.haskell.ui.dialog.dialogfields.*;
import de.leiffrenzel.fp.haskell.ui.util.DefaultStatus;


/** <p>The single page for the 'New Module' wizard.</p> 
  * 
  * @author Leif Frenzel
  */
public class NewModuleWizardPage extends StatusWizardPage {
  
  private StringButtonDialogField dlgFieldSourceFolder;
  private StringButtonDialogField dlgFieldFolders;
  private StringDialogField dlgFieldName;
  
  private IStatus sourceFolderStatus;
  private IStatus folderStatus;
  private IStatus nameStatus;
  
  private ModuleCreationInfo currentInfo;
  
  
  public NewModuleWizardPage() {
    super( "NewModuleWizardPage" );
    setTitle( "Haskell Module" );
    setDescription( "Create a new Haskell module." );
    
    currentInfo = new ModuleCreationInfo();
    
    createDlgFieldSourceFolder();
    createDlgFieldFolder();
    createDlgFieldName();
    
    sourceFolderStatus = new DefaultStatus();
    folderStatus = new DefaultStatus();
    nameStatus = new DefaultStatus();
  }
  
  ModuleCreationOperation getOperation() {        
    return new ModuleCreationOperation( currentInfo );
  }
      
  private void doDialogFieldChanged( final DialogField field ) {
    if( field == dlgFieldSourceFolder ) {
      IContainer sourceContainer = getCurrentlySelectedSourceContainer();
      sourceFolderStatus = Validator.validateSourceFolder( sourceContainer );
      // TODO get sourcefolder from textfield and set it to current info
      // it must be the source container of a Haskell project for that
//      String text = dlgFieldSourceFolder.getText();
//      if( sourceFolderStatus.isOK() ) {
//        currentInfo.setSourceContainer()
//      }
    } else if( field == dlgFieldFolders ) {
      IFolder folder = getCurrentlySelectedFolder();
      if( folder != null ) {
        IPath sourceRelPath = SelectionAnalyzer.getSourceRelativePath( folder );
        currentInfo.setFolders( sourceRelPath );
      }
      String text = dlgFieldFolders.getText();
      folderStatus = Validator.validateFolders( text );
      if( folderStatus.isOK() ) {
        currentInfo.setFolders( new Path( text.replace( '.', '/' ) ) );
      }
    } else if( field == dlgFieldName ) {
      currentInfo.setModuleName( dlgFieldName.getText() );
      nameStatus = Validator.validateModuleName( currentInfo );
    }
    doStatusUpdate();
  }

  private IFolder chooseFolder() {
    FolderSelectionDialog dialog = createFolderSelectionDialog();
    IFolder selection = getCurrentlySelectedFolder();
    if( selection != null ) {
      dialog.setInitialSelections( new Object[] { selection } );
    }
    IFolder result = null;
    if( dialog.open() == Window.OK ) {
      result = ( IFolder )dialog.getFirstResult();
    }
    return result;
  }
  
  
  // interface methods of DialogPage
  //////////////////////////////////

  public void init( final IStructuredSelection selection ) {
    if( selection != null ) {
      IContainer sourceContainer 
        = SelectionAnalyzer.getSourceContainer( selection );
      currentInfo.setSourceContainer( sourceContainer );
      initSourceFolderField( sourceContainer );
      
      IPath path = SelectionAnalyzer.getSourceRelativePath( selection );
      initFolderField( path );
      currentInfo.setFolders( path );
    } 
  }   
  
  public void setVisible( final boolean visible ) {
    super.setVisible( visible );
    if( visible ) {
      initAllStatus();
      dlgFieldName.setFocus();
    }
  }
  
  public void createControl( final Composite parent ) {
    initializeDialogUnits( parent );
    Composite composite = new Composite( parent, SWT.NONE );
    int cols = 4;

    GridLayout layout = new GridLayout();
    layout.numColumns = cols;
    composite.setLayout( layout );

    createSourceFolderControls( composite, cols );
    createFolderControls( composite, cols );
    createSeparator( composite, cols );
    createNameControls( composite, cols );
    setControl( composite );

    Dialog.applyDialogFont( composite );
  }

  
  // UI creation
  //////////////
  
  private void createDlgFieldName() {
    FieldsAdapter adapter = new FieldsAdapter();
    dlgFieldName = new StringDialogField();
    dlgFieldName.setDialogFieldListener( adapter );
    dlgFieldName.setLabelText( "Name" );
  }

  private void createDlgFieldFolder() {
    FieldsAdapter adapter = new FieldsAdapter();
    dlgFieldFolders = new StringButtonDialogField( adapter );
    dlgFieldFolders.setDialogFieldListener( adapter );
    dlgFieldFolders.setLabelText( "Folder" );
    dlgFieldFolders.setButtonLabel( "Browse" );
  }

  private void createDlgFieldSourceFolder() {
    FieldsAdapter adapter = new FieldsAdapter();
    dlgFieldSourceFolder = new StringButtonDialogField( adapter );
    dlgFieldSourceFolder.setDialogFieldListener( adapter );
    dlgFieldSourceFolder.setLabelText( "Source folder" );
    dlgFieldSourceFolder.setButtonLabel( "Browse" );
  }
  
  private void initSourceFolderField( final IContainer sourceContainer ) {
    if( sourceContainer != null ) {
      String content = sourceContainer.getFullPath().toString();
      dlgFieldSourceFolder.setText( content );
    }
  }
  
  private void initFolderField( final IPath path ) {
    if( path != null ) {
      String content = path.toString();
      dlgFieldFolders.setText( content );
    }
  }

  private void createSourceFolderControls( final Composite parent,
                                           final int cols ) {
    dlgFieldSourceFolder.doFillIntoGrid( parent, cols );
    int pixels = convertWidthInCharsToPixels( 40 );
    Text textControl = dlgFieldSourceFolder.getTextControl( null );
    setWidthHint( textControl, pixels );
  }

  private void createFolderControls( final Composite parent, final int cols ) {
    dlgFieldFolders.doFillIntoGrid( parent, cols );
    Text text = dlgFieldFolders.getTextControl( null );
    setWidthHint( text, convertWidthInCharsToPixels( 40 ) );
    Object ld = text.getLayoutData();
    if( ld instanceof GridData ) {
      ( ( GridData )ld ).grabExcessHorizontalSpace = true;
    }
  }
  
  private void createSeparator( final Composite composite, final int cols ) {
    Separator separator = new Separator( SWT.SEPARATOR | SWT.HORIZONTAL );
    int pixels = convertHeightInCharsToPixels( 1 );
    separator.doFillIntoGrid( composite, cols, pixels );   
  }

  private void createNameControls( final Composite parent, final int cols) {
    dlgFieldName.doFillIntoGrid( parent, cols - 1 );
    DialogField.createEmptySpace( parent );
    int pixels = convertWidthInCharsToPixels( 40 );
    setWidthHint( dlgFieldName.getTextControl( null ), pixels );
  }
  
  
  // event handling
  /////////////////
  
  private IContainer chooseSourceFolder( final Object initElement ) {
    Shell shell = getShell();
    SourceFolderSelectionDialog dlg = new SourceFolderSelectionDialog( shell );
    dlg.setInitialSelection( initElement );
    IContainer result = null;
    if( dlg.open() == Window.OK ) {
      Object element = dlg.getFirstResult();
      if( element instanceof IContainer ) {
        result = ( IContainer )element;
      }
    }
    return result;
  }

  private void doChangeControlPressed( final DialogField field ) {
    if( field == dlgFieldFolders ) {
      IFolder folder = chooseFolder();
      if( folder != null ) {
        IPath srcRelPath = SelectionAnalyzer.getSourceRelativePath( folder );
        currentInfo.setFolders( srcRelPath );
        String text = srcRelPath.toString();
        dlgFieldFolders.setText( text.replace( '/', '.' ) );
      }
    } else if( field == dlgFieldSourceFolder ) {
      IContainer oldSourceContainer = currentInfo.getSourceContainer();
      IContainer sourceFolder = chooseSourceFolder( oldSourceContainer );
      if( sourceFolder != null ) {
        initSourceFolderField( sourceFolder );
        currentInfo.setSourceContainer( sourceFolder );
      }
    }
  }
  
  private void doStatusUpdate() {
    IStatus[] status = new IStatus[] {
      sourceFolderStatus,
      folderStatus,
      nameStatus
    };
    // display the most severe status and enable/disable the ok button 
    updateStatus( status );
  }

  
  // helping methods
  //////////////////

  /** returns the folder specified by the contents of the folders textfield,
    * if any, or null else (also if the specified resource does not exist). */
  private IFolder getCurrentlySelectedFolder() {
    IFolder result = null;
    IPath foldersPath = new Path( dlgFieldFolders.getText() );
    IResource resource = getWsRoot().findMember( foldersPath );
    if( resource != null && resource.exists() && resource instanceof IFolder ) {
      result = ( IFolder )resource;
    }
    return result;
  }

  /** returns the folder specified by the contents of the folders textfield,
   * if any, or null else (also if the specified resource does not exist). */
 private IContainer getCurrentlySelectedSourceContainer() {
   IContainer result = null;
   IPath path = new Path( dlgFieldSourceFolder.getText() );
   IResource resource = getWsRoot().findMember( path );
   if(    resource != null 
       && resource.exists() 
       && resource instanceof IContainer ) {
     result = ( IContainer )resource;
   }
   return result;
 }

  private IWorkspaceRoot getWsRoot() {
    return ResourcesPlugin.getWorkspace().getRoot();
  }

  private void initAllStatus() {
    IContainer sourceContainer = currentInfo.getSourceContainer();
    sourceFolderStatus = Validator.validateSourceFolder( sourceContainer );
    folderStatus = Validator.validateFolders( dlgFieldFolders.getText() );
    nameStatus = Validator.validateModuleName( currentInfo );
    doStatusUpdate();
  }
  
  private FolderSelectionDialog createFolderSelectionDialog() {
    IContainer sourceContainer = currentInfo.getSourceContainer();
    return new FolderSelectionDialog( getShell(), sourceContainer );
  }

  private void setWidthHint( final Control control, final int widthHint ) {
    Object ld = control.getLayoutData();
    if( ld instanceof GridData ) {
      ( ( GridData )ld ).widthHint = widthHint;
    }
  }
  
  // inner classes
  ////////////////

  private class FieldsAdapter implements IStringButtonAdapter,
                                         IDialogFieldListener {
    // interface methods of  IStringButtonAdapter
    public void changeControlPressed( final DialogField field ) {
      doChangeControlPressed( field );
    }

    // interface methods of IDialogFieldListener
    public void dialogFieldChanged( final DialogField field ) {
      doDialogFieldChanged( field );
    }
  }
}