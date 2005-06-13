// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.util.Iterator;

import net.sf.eclipsefp.haskell.haddock.core.HaddockInfo;
import net.sf.eclipsefp.haskell.haddock.core.InterfaceListEntry;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/** <p>displays a list of interface files.</p>
  *
  * @author Leif Frenzel
  */
public class InterfaceListBlock extends Composite {

  private CheckboxTableViewer viewer;
  private final HaddockInfo info;
  
  // UI elements
  private Button btnRemove;
  
  
  public InterfaceListBlock( final Composite parent, final HaddockInfo info ) {
    super( parent, SWT.NONE );
    this.info = info;
    setLayout( new GridLayout( 2, false ) );
    
    createListViewer();
    createButtonsBlock();
  }
  
  public void setEnabled( final boolean enabled ) {
    super.setEnabled( enabled );
    viewer.getControl().setEnabled( enabled );
  }
  
  
  // UI creation methods
  //////////////////////
  
  private void createListViewer() {
    viewer = CheckboxTableViewer.newCheckList( this, SWT.BORDER | SWT.MULTI );
    viewer.setLabelProvider( new InterfaceListLP() );
    viewer.setContentProvider( new InterfaceListCP() );
    viewer.setInput( info );
    initializeState();
    
    viewer.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    viewer.addSelectionChangedListener( new ISelectionChangedListener() {
      public void selectionChanged( final SelectionChangedEvent event ) {
        boolean enabled =    event.getSelection() != null 
                          || event.getSelection().isEmpty();
        btnRemove.setEnabled( enabled );
      }
    } );
    viewer.addCheckStateListener( new ICheckStateListener() {
      public void checkStateChanged( final CheckStateChangedEvent event ) {
        Object element = event.getElement();
        ( ( InterfaceListEntry )element ).setUsed( event.getChecked() );
      }
    } );    
  }

  private void createButtonsBlock() {
    Composite buttonBar = createButtonBar();

    Button btnAdd = createButton( buttonBar, "Add" );
    btnAdd.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        handleAdd();
      }
    } );
    btnRemove = createButton( buttonBar, "Remove" );
    btnRemove.setEnabled( false );
    btnRemove.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        handleRemove();
      }
    } );
    createEmptySpace( buttonBar );
    createEmptySpace( buttonBar );
    Button btnSelectAll = createButton( buttonBar, "Select all" );
    btnSelectAll.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        info.getReadInterfaceFiles().selectAll();
        viewer.setAllChecked( true );
      }
    } );
    Button btnDeselectAll = createButton( buttonBar, "Deselect all" );
    btnDeselectAll.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        info.getReadInterfaceFiles().deselectAll();
        viewer.setAllChecked( false );
      }
    } );
  }
  
  private Button createButton( final Composite buttonBar, final String text ) {
    Button result = new Button( buttonBar, SWT.NONE );
    result.setText( text );
    result.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    return result;
  }

  private Composite createButtonBar() {
    Composite buttonBar = new Composite( this, SWT.NONE );
    buttonBar.setLayout( new GridLayout( 1, false ) );
    GridData gridData = new GridData();
    gridData.verticalAlignment = SWT.TOP;
    buttonBar.setLayoutData( gridData );
    return buttonBar;
  }
  
  private void createEmptySpace( final Composite parent ) {
    Label label = new Label( parent, SWT.NULL );
    GridData gd = new GridData( GridData.VERTICAL_ALIGN_BEGINNING );
    gd.widthHint = 0;
    gd.heightHint = 0;
    label.setLayoutData( gd );
  }

  
  // helping methods
  //////////////////
  
  private void handleAdd() {
    FileDialog fd = new FileDialog( getShell() );
    String result = fd.open();
    if( result != null && !result.trim().equals( "" ) ) {
      InterfaceListEntry entry = new InterfaceListEntry( result, true );
      info.getReadInterfaceFiles().add( entry );
      viewer.refresh();
      viewer.setChecked( entry, entry.isUsed() );
    }
  }

  private void handleRemove() {
    ISelection selection = viewer.getSelection();
    if( selection != null ) {
      IStructuredSelection ssel = ( IStructuredSelection )selection;
      Iterator iter = ssel.iterator();
      while( iter.hasNext() ) {
        InterfaceListEntry entry = ( InterfaceListEntry )iter.next();
        info.getReadInterfaceFiles().remove( entry );
      }
      viewer.refresh();
    }
  }

  private void initializeState() {
    InterfaceListEntry[] entries = info.getReadInterfaceFiles().getAll();
    for( int i = 0; i < entries.length; i++ ) {
      viewer.setChecked( entries[ i ], entries[ i ].isUsed() );
    }
  }
  
  // inner classes
  ////////////////
  
  private class InterfaceListLP extends LabelProvider {
    public Image getImage( final Object element ) {
      String key = ISharedImages.IMG_OBJ_FILE;
      return PlatformUI.getWorkbench().getSharedImages().getImage( key );
    }

    public String getText( final Object element ) {
      return ( ( InterfaceListEntry )element ).getFileName();
    }
  }

  private class InterfaceListCP implements IStructuredContentProvider {
    
    private HaddockInfo info;
    
    
    // interface methods of IStructuredContentProvider
    //////////////////////////////////////////////////
    
    public Object[] getElements( final Object inputElement ) {
      return info.getReadInterfaceFiles().getAll();
    }

    public void dispose() {
      // unused
    }

    public void inputChanged( final Viewer viewer, 
                              final Object oldInput, 
                              final Object newInput ) {
      info = ( HaddockInfo )newInput;
    }
  }
}
