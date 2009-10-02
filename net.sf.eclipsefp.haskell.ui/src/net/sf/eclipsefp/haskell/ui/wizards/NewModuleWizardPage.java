// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.core.code.EHaskellCommentStyle;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.dialog.FolderSelectionDialog;
import net.sf.eclipsefp.haskell.ui.dialog.SourceFolderSelectionDialog;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.DialogField;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.IDialogFieldListener;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.IStringButtonAdapter;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.Separator;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringButtonDialogField;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringDialogField;
import net.sf.eclipsefp.haskell.ui.util.DefaultStatus;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;


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

  private final ModuleCreationInfo currentInfo;
  private Button chkUseLiterate;
  private Group grpLiterate;
  private Button rdoLiterate;
  private Button rdoTex;


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

  ModuleCreationInfo getInfo() {
    if (chkUseLiterate.getSelection()) {
      if (rdoLiterate.getSelection()) {
        currentInfo.setCommentStyle(EHaskellCommentStyle.LITERATE);
      } else {
        currentInfo.setCommentStyle(EHaskellCommentStyle.TEX);
      }
    } else {
      currentInfo.setCommentStyle(EHaskellCommentStyle.USUAL);
    }
    return currentInfo;
    //return new ModuleCreationOperation( currentInfo );
  }

  private void doDialogFieldChanged( final DialogField field ) {
    if( field == dlgFieldSourceFolder ) {
      IContainer sourceContainer = getCurrentlySelectedSourceContainer();
      sourceFolderStatus = Validator.validateSourceFolder( sourceContainer );
      // do we care if it's not a source folder as such?
      if( sourceFolderStatus.isOK() ) {
        currentInfo.setSourceContainer(sourceContainer);
      }
    } else if( field == dlgFieldFolders ) {
      IFolder folder = getCurrentlySelectedFolder();
      if( folder != null ) {
        IPath sourceRelPath = ResourceUtil.getSourceRelativePath( folder );
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

  @Override
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
    createLiterateControls( composite );

    setControl( composite );

    Dialog.applyDialogFont( composite );
  }


  private void createLiterateControls( final Composite composite ) {
    createUseLiterateCheckBox( composite );
    createLiterateBlock(composite);
  }

  private void createUseLiterateCheckBox( final Composite composite ) {
    GridData gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = false;
    gd.horizontalSpan = 1;
    chkUseLiterate = new Button(composite, SWT.CHECK);
    chkUseLiterate.setText( "Use literate style" );
    chkUseLiterate.setLayoutData(gd);

    chkUseLiterate.addSelectionListener(new SelectionListener() {

      public void widgetSelected( final SelectionEvent e ) {
        enableLiterateGroup(chkUseLiterate.getSelection());
      }

      public void widgetDefaultSelected( final SelectionEvent e ) {
        widgetSelected( e );
      }

    });
  }

  private void createLiterateBlock( final Composite composite ) {
    createLiterateGroup( composite );
    createLiterateRadio();
    createTexStyleRadio();
  }

  private void createLiterateGroup( final Composite composite ) {
    grpLiterate = new Group(composite, SWT.NONE);
    grpLiterate.setText("Literate Haskell");
    grpLiterate.setEnabled( false );
    GridData gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = false;
    gd.horizontalSpan = 4;
    grpLiterate.setLayoutData(gd);
    GridLayout layout= new GridLayout();
    layout.marginHeight= 0;
    layout.marginWidth= 0;
    layout.numColumns= 1;
    grpLiterate.setLayout(layout);
  }

  private void createLiterateRadio() {
    rdoLiterate = new Button(grpLiterate, SWT.RADIO);
    rdoLiterate.setText("Traditional literate style");
    rdoLiterate.setEnabled( false );
    rdoLiterate.setSelection(true);
  }

  private void createTexStyleRadio() {
    rdoTex = new Button(grpLiterate, SWT.RADIO);
    rdoTex.setText("Tex literate style");
    rdoTex.setEnabled( false );
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
      dlgFieldFolders.setText( content.replace( '/', '.' ) );
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
        IPath srcRelPath = ResourceUtil.getSourceRelativePath( folder );
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
    IPath foldersPath = new Path( dlgFieldFolders.getText().replace( '.', '/' ) );
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

  private void enableLiterateGroup(final boolean enabled) {
    grpLiterate.setEnabled(enabled);
    for(Control child : grpLiterate.getChildren()) {
      child.setEnabled( enabled );
    }
  }

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