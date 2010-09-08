// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.hsimpls;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import net.sf.eclipsefp.haskell.core.internal.hsimpl.HsImplementation;
import net.sf.eclipsefp.haskell.core.internal.hsimpl.HsImplementationPersister;
import net.sf.eclipsefp.haskell.core.internal.hsimpl.IHsImplementation;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.util.UIUtils;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

/** <p>A composite that displays installed Haskell implementations in a
  * table. Implementations can be added, removed and edited.</p>
  *
  * @author Leif Frenzel
  */
class ImplementationsBlock implements ISelectionProvider {

  private static final String KEY_COLUMN_WIDTH = ".columnWidth"; //$NON-NLS-1$
  private static final String KEY_SORT_COLUMN = ".sortColumn"; //$NON-NLS-1$
  private static final int DEFAULT_WIDTH = 100;

  private final List<IHsImplementation> installations
    = new ArrayList<IHsImplementation>();

  private CheckboxTableViewer viewer;
  private Table table;
  private Button btnAdd;
  private Button btnRemove;
  private Button btnEdit;

  // index of column used for sorting
  private int sortColumn = 0;
  private final ListenerList selectionListeners = new ListenerList();
  private ISelection lastSelection = new StructuredSelection();

  Composite createControl( final Composite parent ) {
    Composite composite = UIUtils.createMainComposite( parent );

    Label tableLabel = new Label( composite, SWT.NONE );
    tableLabel.setText( UITexts.implementationsBlock_installed );
    GridData data = new GridData();
    data.horizontalSpan = 2;
    tableLabel.setLayoutData( data );
    tableLabel.setFont( parent.getFont() );

    table = UIUtils.createTable( composite );
    table.addKeyListener( new KeyAdapter() {
      @Override
      public void keyPressed(final KeyEvent event) {
        if( event.character == SWT.DEL && event.stateMask == 0 ) {
          removeSelectedInstallations();
        }
      }
    } );
    createColumns();
    createViewer();

    Composite buttonsComp = createButtonsComposite( composite );
    createButtons( buttonsComp );
    createSpacer( buttonsComp );

    enableButtons();
    return composite;
  }

  private void sortByType() {
    viewer.setComparator( new Comparator_TypeName() );
    sortColumn = 3;
  }

  private void sortByName() {
    viewer.setComparator( new Comparator_Name() );
    sortColumn = 1;
  }

  private void sortByVersion() {
    viewer.setComparator( new Comparator_Version() );
    sortColumn = 2;
  }

  void applyPref( final String prefValue ) {
    HsImplementationPersister.fromXML( prefValue, installations );
    viewer.setInput( installations );
    viewer.refresh();
  }

  String getPref() {
    return HsImplementationPersister.toXML( installations );
  }

  void add( final HsImplementation impl ) {
    installations.add( impl );
    viewer.setInput( installations );
    viewer.refresh();
  }

  boolean isDuplicateName( final String name, final HsImplementation impl ) {
    boolean result = false;
    if( name != null && name.trim().length() > 0 ) {
      for( IHsImplementation inst: installations ) {
        result |= inst!=impl && name.equals( inst.getName() );
      }
    }
    return result;
  }

  void setCheckedHsImplementation( final String name ) {
    IHsImplementation impl = findImpl( name );
    if( impl == null ) {
      setSelection( new StructuredSelection() );
    } else {
      setSelection( new StructuredSelection( impl ) );
    }
  }

  private IHsImplementation findImpl( final String name ) {
    IHsImplementation result = null;
    for( IHsImplementation impl: installations ) {
      if( impl.getName().equals( name ) ) {
        result = impl;
      }
    }
    return result;
  }

  IHsImplementation getCheckedHsImplementation() {
    IHsImplementation result = null;
    Object[] objects = viewer.getCheckedElements();
    if( objects.length > 0 ) {
      result = ( IHsImplementation )objects[ 0 ];
    }
    return result;
  }

  void setInstallations( final IHsImplementation[] insts ) {
    installations.clear();
    for( int i = 0; i < insts.length; i++ ) {
      installations.add( insts[ i ] );
    }
    viewer.setInput( installations );
    viewer.refresh();
  }

  void saveColumnSettings( final IDialogSettings settings,
                           final String qualifier ) {
    int columnCount = table.getColumnCount();
    for( int i = 0; i < columnCount; i++ ) {
      int width = table.getColumn( i ).getWidth();
      settings.put( qualifier + KEY_COLUMN_WIDTH + i, width );
    }
    settings.put( qualifier + KEY_SORT_COLUMN, sortColumn );
  }

  void restoreColumnSettings( final IDialogSettings settings,
                              final String qualifier ) {
    viewer.getTable().layout( true );
    restoreColumnWidths( settings, qualifier );
    try {
      sortColumn = settings.getInt( qualifier + KEY_SORT_COLUMN );
    } catch ( final NumberFormatException numfex ) {
      sortColumn = 1;
    }
    switch( sortColumn ) {
      case 1:
        sortByName();
        break;
      case 2:
        sortByVersion();
        break;
      case 3:
        sortByType();
        break;
      }
  }

  // interface methods of ISelectionProvider
  //////////////////////////////////////////

  public void addSelectionChangedListener( final ISelectionChangedListener li ) {
    selectionListeners.add( li );
  }

  public ISelection getSelection() {
    return new StructuredSelection( viewer.getCheckedElements() );
  }

  public void removeSelectionChangedListener( final ISelectionChangedListener li ) {
    selectionListeners.remove( li );
  }

  public void setSelection( final ISelection selection ) {
    if( selection instanceof IStructuredSelection ) {
      if( !selection.equals( lastSelection ) ) {
        lastSelection = selection;
        Object elem = ( ( IStructuredSelection )selection ).getFirstElement();
        if( elem == null ) {
          viewer.setCheckedElements( new Object[ 0 ] );
        } else {
          viewer.setCheckedElements( new Object[] { elem } );
          viewer.reveal( elem );
        }
        fireSelectionChanged();
      }
    }
  }


  // helping functions
  ////////////////////

  private void fireSelectionChanged() {
    SelectionChangedEvent evt = new SelectionChangedEvent( this, getSelection() );
    Object[] lis = selectionListeners.getListeners();
    for( int i = 0; i < lis.length; i++ ) {
      ISelectionChangedListener li = ( ISelectionChangedListener )lis[ i ];
      li.selectionChanged( evt );
    }
  }

  private void enableButtons() {
    IStructuredSelection ssel = (IStructuredSelection) viewer.getSelection();
    btnEdit.setEnabled( ssel.size() == 1 );
    boolean moreThanSelectedItem
      = ssel.size() > 0 && ssel.size() < viewer.getTable().getItemCount();
    btnRemove.setEnabled( moreThanSelectedItem );
  }

  private void createColumns() {
    createColumn( UITexts.implementationsBlock_colName, new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        sortByName();
      }
    } );

    createColumn( UITexts.implementationsBlock_colType,
                  new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        sortByType();
      }
    } );

    createColumn( UITexts.implementationsBlock_colVersion,
                  new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        sortByVersion();
      }
    } );
  }

  private TableColumn createColumn( final String text,
                                    final SelectionListener listener ) {
    TableColumn result = new TableColumn( table, SWT.NULL );
    result.setText( text );
    result.addSelectionListener( listener );
    return result;
  }

  private Composite createButtonsComposite( final Composite parent ) {
    Composite result = new Composite( parent, SWT.NULL );
    result.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_BEGINNING ) );
    GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    result.setLayout( gridLayout );
    result.setFont( parent.getFont() );
    return result;
  }

  private void createButtons( final Composite buttonsComp ) {
    String sAdd = UITexts.implementationsBlock_btnAdd;
    btnAdd = SWTUtil.createPushButton( buttonsComp, sAdd );
    btnAdd.addListener( SWT.Selection, new Listener() {
      public void handleEvent( final Event evt ) {
        addHsImplementation();
      }
    } );

    String sEdit = UITexts.implementationsBlock_btnEdit;
    btnEdit = SWTUtil.createPushButton( buttonsComp, sEdit );
    btnEdit.addListener( SWT.Selection, new Listener() {
      public void handleEvent( final Event evt ) {
        editHsImplementation();
      }
    } );

    String sRemove = UITexts.implementationsBlock_btnRemove;
    btnRemove = SWTUtil.createPushButton( buttonsComp, sRemove );
    btnRemove.addListener( SWT.Selection, new Listener() {
      public void handleEvent( final Event evt ) {
        removeSelectedInstallations();
      }
    } );
  }

  private void createViewer() {
    viewer = new CheckboxTableViewer( table );
    viewer.setLabelProvider( new HsImplementationsLP() );
    viewer.setContentProvider( new HsImplementationsCP( installations ) );
    // by default, sort by name
    sortByName();

    viewer.addSelectionChangedListener( new ISelectionChangedListener() {
      public void selectionChanged( final SelectionChangedEvent evt ) {
        enableButtons();
      }
    } );

    viewer.addCheckStateListener( new ICheckStateListener() {
      public void checkStateChanged( final CheckStateChangedEvent event ) {
        if( event.getChecked() ) {
          IHsImplementation element
            = ( IHsImplementation )event.getElement();
          setCheckedHsImplementation( element.getName() );
        } else {
          setCheckedHsImplementation( null );
        }
      }
    } );

    viewer.addDoubleClickListener( new IDoubleClickListener() {
      public void doubleClick( final DoubleClickEvent e ) {
        if( !viewer.getSelection().isEmpty() ) {
          editHsImplementation();
        }
      }
    } );
  }

  private void restoreColumnWidths( final IDialogSettings settings,
                                    final String qualifier ) {
    int columnCount = table.getColumnCount();
    for( int i = 0; i < columnCount; i++ ) {
      int width = -1;

      try {
        width = settings.getInt( qualifier + KEY_COLUMN_WIDTH + i );
      } catch( final NumberFormatException numfex ) {
        // ignored
      }

      if( width <= 0 ) {
        //table.getColumn( i ).pack();
        table.getColumn( i ).setWidth( DEFAULT_WIDTH );
      } else {
        table.getColumn( i ).setWidth( width );
      }
    }
  }

  private void createSpacer( final Composite buttonsComp ) {
    Label separator = new Label( buttonsComp, SWT.NONE );
    separator.setVisible( false );
    GridData gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.verticalAlignment = GridData.BEGINNING;
    gd.heightHint = 4;
    separator.setLayoutData( gd );
  }

  private void removeSelectedInstallations() {
    IStructuredSelection ssel = ( IStructuredSelection )viewer.getSelection();
    IHsImplementation[] insts
      = new IHsImplementation[ ssel.size() ];
    Iterator iter = ssel.iterator();
    int i = 0;
    while( iter.hasNext() ) {
      insts[ i ] = ( IHsImplementation )iter.next();
      i++;
    }
    removeHsImplementations( insts );
  }

  private void removeHsImplementations( final IHsImplementation[] insts ) {
    IStructuredSelection prev = ( IStructuredSelection )getSelection();
    for( int i = 0; i < insts.length; i++ ) {
      installations.remove( insts[ i ] );
    }
    viewer.refresh();
    autoSelectSingle( prev );
  }

  private void addHsImplementation() {
    IStructuredSelection prev = ( IStructuredSelection )getSelection();
    HsImplementationDialog dialog
      = new HsImplementationDialog( table.getShell(), this, null );
    dialog.setTitle( UITexts.implementationsBlock_dlgAdd );
    if( dialog.open() == Window.OK ) {
      add( dialog.getResult() );
      autoSelectSingle( prev );
    }
  }

  private void editHsImplementation() {
    IStructuredSelection ssel = ( IStructuredSelection )viewer.getSelection();
    if( !ssel.isEmpty() ) {
      HsImplementation impl = ( HsImplementation )ssel.getFirstElement();
      Shell shell = table.getShell();
      HsImplementationDialog dlg = new HsImplementationDialog( shell, this, impl );
      dlg.setTitle( UITexts.implementationsBlock_dlgEdit );
      if( dlg.open() == Window.OK ) {
        installations.remove( impl );
        add( dlg.getResult() );
        autoSelectSingle( ssel );
      }
    }
  }

  private void autoSelectSingle( final IStructuredSelection prev ) {
    IStructuredSelection curr = ( IStructuredSelection )getSelection();
    if( !curr.equals( prev ) || curr.isEmpty() ) {
      if( curr.size() == 0 && installations.size() == 1 ) {
        // pick a default automatically
        setSelection( new StructuredSelection( installations.get( 0 ) ) );
      }
    } else {
      fireSelectionChanged();
    }
  }

  private final class Comparator_Version extends ViewerComparator {


    // interface methods of ViewerComparator
    ////////////////////////////////////////

    @Override
    public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
      int result = super.compare( viewer, e1, e2 );
      if(    ( e1 instanceof IHsImplementation )
          && ( e2 instanceof IHsImplementation ) ) {
        IHsImplementation left = ( IHsImplementation )e1;
        IHsImplementation right = ( IHsImplementation )e2;
        result = left.getVersion().compareTo( right.getVersion() );
      }
      return result;
    }

    @Override
    public boolean isSorterProperty( final Object element,
        final String property ) {
      return true;
    }
  }

  private final class Comparator_Name extends ViewerComparator {

    // interface methods of ViewerComparator
    ////////////////////////////////////////

    @Override
    public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
      int result = super.compare( viewer, e1, e2 );
      if(    ( e1 instanceof IHsImplementation )
          && ( e2 instanceof IHsImplementation ) ) {
        IHsImplementation left = ( IHsImplementation )e1;
        IHsImplementation right = ( IHsImplementation )e2;
        result = left.getName().compareToIgnoreCase( right.getName() );
      }
      return result;
    }

    @Override
    public boolean isSorterProperty( final Object element,
        final String property ) {
      return true;
    }
  }

  private final class Comparator_TypeName extends ViewerComparator {

    // interface methods of ViewerComparator
    ////////////////////////////////////////

    @Override
    public int compare( final Viewer viewer,
                        final Object e1,
                        final Object e2 ) {
      int result = super.compare( viewer, e1, e2 );
      if( e1 instanceof IHsImplementation && e2 instanceof IHsImplementation ) {
        IHsImplementation left = ( IHsImplementation )e1;
        IHsImplementation right = ( IHsImplementation )e2;
        String leftType = left.getType().name();
        String rightType = right.getType().name();
        if( leftType.compareToIgnoreCase( rightType ) != 0 ) {
          result = leftType.compareToIgnoreCase( rightType );
        } else {
          result = left.getName().compareToIgnoreCase( right.getName() );
        }
      }
      return result;
    }

    @Override
    public boolean isSorterProperty( final Object elem, final String prop ) {
      return true;
    }
  }
}
