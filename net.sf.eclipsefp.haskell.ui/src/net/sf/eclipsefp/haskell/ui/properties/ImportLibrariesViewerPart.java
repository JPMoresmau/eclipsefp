// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import net.sf.eclipsefp.haskell.core.project.ImportLibrariesList;
import net.sf.eclipsefp.haskell.ui.properties.viewerpart.CheckboxTablePart;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;


/** <p>special viewer part with buttons for the import libraries 
  * preference page.</p>
  * 
  * @author Leif Frenzel
  */
class ImportLibrariesViewerPart extends CheckboxTablePart {

  private static DirectoryDialog dialog;
  
  public ImportLibrariesViewerPart() {
    super( new String[] { "Add...",
                          "Remove",
                          null,
                          null,
                          "Select All",
                          "Deselect All" } );
  }

  @Override
  protected void buttonSelected( final Button button, final int index ) {
    switch( index ) {
      case 0:
        handleAdd();
        break;
      case 1:
        handleDelete();
        break;
      case 4:
        selectAll( true );
        break;
      case 5:
        selectAll( false );
        break;
    }
  }

  @Override
  protected Button createButton( final Composite parent, 
                                 final String label, 
                                 final int index ) {
    Button button = super.createButton( parent, label, index );
    SWTUtil.setButtonDimensionHint( button );
    return button;
  }

  @Override
  protected void createMainLabel( final Composite parent, final int span ) {
    Label label = new Label( parent, SWT.NULL );
    GridData gd = new GridData( GridData.FILL );
    gd.horizontalSpan = span;
    label.setLayoutData( gd );
  }

  @Override
  protected void selectionChanged( final IStructuredSelection selection ) {
    setButtonEnabled( 1, !selection.isEmpty() );
  }

  @Override
  protected void elementChecked( final Object element, final boolean checked ) {
    ( ( IImportLibrary )element ).setUsed( checked );
  }

  
  private void selectAll( final boolean selected ) {
    IImportLibrary[] libs = getImportLibList().getAll();
    for( int i = 0; i < libs.length; i++ ) {
      libs[ i ].setUsed( selected );
    }
    getTableViewer().setAllChecked( selected );
  }

  private ImportLibrariesList getImportLibList() {
    return ( ImportLibrariesList )getTableViewer().getInput();
  }

  private void handleAdd() {
    String path = getDirectoryDialog( null ).open();
    if( path != null ) {
      IImportLibrary lib = getImportLibList().createLibrary( path, true );
      if( !getImportLibList().contains( lib ) ) {
        getImportLibList().add( lib );
        getTableViewer().add( lib );
        getTableViewer().setChecked( lib, lib.isUsed() );
      }
    }
  }

  private DirectoryDialog getDirectoryDialog( final String filterPath ) {
    if( dialog == null ) {
      IWorkbench workbench = PlatformUI.getWorkbench();
      Shell shell = workbench.getActiveWorkbenchWindow().getShell();
      dialog = new DirectoryDialog( shell );
    }
    dialog.setMessage( "Choose an import library location:" );
    if( filterPath != null ) {
      dialog.setFilterPath( filterPath );
    }
    return dialog;
  }

  private void handleDelete() {
    ISelection sel = getTableViewer().getSelection();
    IStructuredSelection selection = ( IStructuredSelection )sel;
    IImportLibrary lib = ( IImportLibrary )selection.getFirstElement();
    getImportLibList().remove( lib );
    getTableViewer().remove( lib );
  }
}