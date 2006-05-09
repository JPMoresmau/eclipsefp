// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PropertyPage;

import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import net.sf.eclipsefp.haskell.core.project.ImportLibrariesList;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.properties.viewerpart.CheckboxTablePart;

/** <p>The property page for import libraries on Haskell projects.</p>
  * 
  * @author Leif Frenzel
  */
public class ImportLibrariesPP extends PropertyPage
                               implements IWorkbenchPreferencePage {
  
  private final CheckboxTablePart tablePart;
  private ImportLibrariesList list;

  public ImportLibrariesPP() {
    tablePart = new ImportLibrariesViewerPart();
    setDescription( "Locations that will be searched for import libraries:" );
  }


  // interface methods of IDialogPage
  ///////////////////////////////////
  
  public Control createContents( final Composite parent ) {
    Composite container = new Composite( parent, SWT.NULL );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    container.setLayout( layout );
    tablePart.setMinimumSize( 150, 200 );
    tablePart.createControl( container, SWT.BORDER, 2 );

    list = new ImportLibrariesList( ( IProject )getElement() );
    ImportLibrariesContentProvider cp = new ImportLibrariesContentProvider();
    tablePart.getTableViewer().setContentProvider( cp );
    ImportLibrariesLabelProvider lp = new ImportLibrariesLabelProvider();
    tablePart.getTableViewer().setLabelProvider( lp );
    tablePart.getTableViewer().setInput( list );
    initializeStates();
    
    tablePart.setButtonEnabled( 1, false );
    tablePart.setButtonEnabled( 2, false );
    Dialog.applyDialogFont( parent );
    return container;
  }
  
  
  // interface methods of IPreferencePage / IWorkbenchPreferencePage
  //////////////////////////////////////////////////////////////////
  
  public void init( final IWorkbench workbench ) {
    // unused
  }

  public boolean performOk() {
    try {
      list.save();
    } catch( CoreException ex ) {
      HaskellUIPlugin.log( "Could not save import libraries list.", ex );
    }
    return super.performOk();
  }

  public void performDefaults() {
    // uncheck all libs by default
    IImportLibrary[] libs = list.getAll();
    for( int i = 0; i < libs.length; i++ ) {
      libs[ i ].setUsed( false );
      tablePart.getTableViewer().setChecked( libs[ i ], false );
    }
    tablePart.getTableViewer().refresh();
    super.performDefaults();
  }

  
  // helping methods
  //////////////////
  
  ImportLibrariesList getImportLibList() {
    return list;
  }
  
  private void initializeStates() {
    IImportLibrary[] libs = list.getAll();
    for( int i = 0; i < libs.length; i++ ) {
      tablePart.getTableViewer().setChecked( libs[ i ], libs[ i ].isUsed() );
    }
  }
}