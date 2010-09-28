// Copyright (c) 2010, B. Scott Michel
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.layout.TableColumnLayout;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

/** The Cabal implementations table and associated buttons, dialogs and UI
 * niceties.
 *
 * @author B. Scott Michel
 */
public class CabalImplsBlock implements ISelectionProvider {
  private final static String KEY_SORT_COLUMN = ".sortColumn";
  private final static String KEY_COLUMN_WIDTH = ".columnWidth";

  private CheckboxTableViewer viewer;
  private int sortColumn;
  private Table table;
  private Button btnAdd;
  private Button btnRemove;
  private Button btnEdit;
  private Button btnAutoDetect;
  private final List<CabalImplementation> impls = new ArrayList<CabalImplementation>();
  private final ListenerList selectionListeners = new ListenerList();

  Composite createControl( final Composite parent, final PreferencePage prefParent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    Font parentFont = parent.getFont();

    GridLayout glayout = new GridLayout(2, false);
    glayout.marginHeight = 0;
    glayout.marginWidth = 0;
    composite.setLayout( glayout );
    composite.setFont( parentFont );

    Label tableLabel = new Label( composite, SWT.NONE );
    tableLabel.setText( UITexts.cabalImplsBlock_installed );
    GridData gdata = new GridData( SWT.FILL, SWT.TOP, true, false );
    gdata.horizontalSpan = 2;
    tableLabel.setLayoutData( gdata );
    tableLabel.setFont( parentFont );

    Composite tableComposite = new Composite ( composite, SWT.NONE );
    tableComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true) );

    table = SWTUtil.createTable( tableComposite );
    createColumns( tableComposite );
    createViewer();

    // Deep copy the implementations, replace them later.
    CabalImplementationManager cMgr = CabalImplementationManager.getInstance();
    impls.clear();
    for (CabalImplementation impl : cMgr.getCabalImplementations()) {
      impls.add( new CabalImplementation( impl ) );
    }

    viewer.setInput( impls );

    // And set the current default (checked) implementation
    CabalImplementation defImpl = cMgr.getDefaultCabalImplementation();
    if (defImpl != null) {
      CabalImplementation impl = findImplementation( defImpl.getUserIdentifier() );
      setCheckedCabalImplementation( impl );
    }

    Composite buttonsComp = new Composite( composite, SWT.NONE );

    glayout = new GridLayout(1, true);
    glayout.marginHeight = 0;
    glayout.marginWidth = 0;
    buttonsComp.setLayout( glayout );
    buttonsComp.setLayoutData( new GridData( SWT.CENTER, SWT.TOP, false, false ) );
    buttonsComp.setFont( parentFont );
    createButtons( buttonsComp );
    enableButtons();

    return composite;
  }

  /** Put the Cabal implementation preferences into the preference store. */
  public boolean updateCabalImplementations(  ) {
    CabalImplementationManager cMgr = CabalImplementationManager.getInstance();
    cMgr.setCabalImplementations( impls );
    IStructuredSelection sel = (IStructuredSelection) viewer.getSelection();
    CabalImplementation impl = (CabalImplementation) sel.getFirstElement();
    if (impl != null) {
      cMgr.setDefaultCabalImplementation( impl.getUserIdentifier(), true );
    }
    return true;
  }

  public boolean validate( final PreferencePage parent ) {
    if( impls.size() <= 0 ) {
      parent.setErrorMessage( UITexts.cabalImplsBlock_noCabalInstallations );
      return false;
    } else {
      CabalImplementation install = getCheckedCabalImplementation();
      if( install == null ) {
        parent.setErrorMessage( UITexts.cabalImplsBlock_noCabalInstallationSelected );
        return false;
      }
    }

    return true;
  }

  // +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  // Internal helper methods
  // +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  private void add(final CabalImplementation impl) {
    impls.add( impl );
  }

  private void update (final String identifier, final CabalImplementation theImpl) {
    CabalImplementation impl = findImplementation( identifier );
    if (impl != null) {
      // Update the fields.
      impl.copy (theImpl);
    }
  }

  public void addSelectionChangedListener( final ISelectionChangedListener listener ) {
    selectionListeners.add( listener );
  }

  public ISelection getSelection() {
    return new StructuredSelection( viewer.getCheckedElements() );
  }

  public CabalImplementation getCheckedCabalImplementation() {
    CabalImplementation result = null;
    Object[] objects = viewer.getCheckedElements();
    if( objects.length > 0 ) {
      result = ( CabalImplementation ) objects[ 0 ];
    }
    return result;
  }


  public void removeSelectionChangedListener( final ISelectionChangedListener listener ) {
    selectionListeners.remove( listener );
  }

  public void setSelection( final ISelection selection ) {
    if( selection instanceof IStructuredSelection ) {
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

  private void setCheckedCabalImplementation( final String identifier ) {
    setCheckedCabalImplementation( findImplementation( identifier ) );
  }

  private void setCheckedCabalImplementation( final CabalImplementation impl ) {
    if( impl == null ) {
      setSelection( new StructuredSelection() );
    } else {
      setSelection( new StructuredSelection( impl ) );
    }
    fireSelectionChanged();
  }

  private CabalImplementation findImplementation( final String ident ) {
    CabalImplementation retval = null;
    for (CabalImplementation impl : impls) {
      if (impl.getUserIdentifier().equals( ident )) {
        retval = impl;
        break;
      }
    }
    return retval;
  }

  // Helper methods:
  private void createColumns(final Composite composite) {
    TableColumn colName = createColumn( UITexts.cabalImplsBlock_colName, new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        sortByUserIdentifier();
      }
    } );
    TableColumn colInstallVersion = createColumn (UITexts.cabalImplsBlock_colCabalInstallVersion, new SelectionAdapter() {
      @Override
      public void widgetSelected ( final SelectionEvent evt ) {
        sortByInstallVersion();
      }
    } );
    TableColumn colLibraryVersion = createColumn (UITexts.cabalImplsBlock_colCabalLibraryVersion, new SelectionAdapter() {
      @Override
      public void widgetSelected ( final SelectionEvent evt ) {
        sortByLibraryVersion();
      }
    } );
    TableColumn colCabalPath = createColumn (UITexts.cabalImplsBlock_colCabalPath, new SelectionAdapter() {
      @Override
      public void widgetSelected ( final SelectionEvent evt ) {
        sortByExecutablePath();
      }
    });

    TableColumnLayout tcLayout = new TableColumnLayout();
    composite.setLayout( tcLayout );

    tcLayout.setColumnData( colName, new ColumnWeightData( 25, true ) );
    tcLayout.setColumnData( colInstallVersion, new ColumnWeightData( 20, true ) );
    tcLayout.setColumnData( colLibraryVersion, new ColumnWeightData( 20, true ) );
    tcLayout.setColumnData( colCabalPath, new ColumnWeightData( 35, true ) );
  }

  private TableColumn createColumn( final String text, final SelectionListener listener ) {
    TableColumn result = new TableColumn( table, SWT.NONE );
    result.setText( text );
    result.addSelectionListener( listener );
    return result;
  }

  private void createButtons( final Composite buttonsComp ) {
    String sAdd = UITexts.cabalImplsBlock_btnAdd;
    btnAdd = SWTUtil.createPushButton( buttonsComp, sAdd );
    btnAdd.addListener( SWT.Selection, new Listener() {
      public void handleEvent( final Event evt ) {
        addCabalImplementation();
      }
    } );

    String sEdit = UITexts.implementationsBlock_btnEdit;
    btnEdit = SWTUtil.createPushButton( buttonsComp, sEdit );
    btnEdit.addListener( SWT.Selection, new Listener() {
      public void handleEvent( final Event evt ) {
        editCabalImplementation();
      }
    } );

    String sRemove = UITexts.cabalImplsBlock_btnRemove;
    btnRemove = SWTUtil.createPushButton( buttonsComp, sRemove );
    btnRemove.addListener( SWT.Selection, new Listener() {
      public void handleEvent( final Event evt ) {
        removeSelectedCabalImplementations();
      }
    } );

    String sDetect = UITexts.cabalImplsBlock_btnAutoDetect;
    btnAutoDetect = SWTUtil.createPushButton( buttonsComp, sDetect );
    btnAutoDetect.addListener( SWT.Selection, new Listener() {
      public void handleEvent (final Event ev) {
        autoDetectCabalImpls();
      }
    });
  }

  private void createViewer() {
    viewer = new CheckboxTableViewer( table );
    viewer.setLabelProvider( new CabalImplsLP() );
    viewer.setContentProvider( new CabalImplsCP( impls ) );
    sortByUserIdentifier();

    viewer.addSelectionChangedListener( new ISelectionChangedListener() {
      public void selectionChanged( final SelectionChangedEvent evt ) {
        enableButtons();
      }
    } );

    viewer.addCheckStateListener( new ICheckStateListener() {
      public void checkStateChanged( final CheckStateChangedEvent event ) {

        CabalImplementation element = ( CabalImplementation ) event.getElement();
        if (!event.getChecked()) {
          element = null;
        }
        setCheckedCabalImplementation( element );
      }
    } );

    viewer.addDoubleClickListener( new IDoubleClickListener() {
      public void doubleClick( final DoubleClickEvent e ) {
        if( !viewer.getSelection().isEmpty() ) {
          editCabalImplementation();
        }
      }
    } );

  }

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
    boolean moreThanSelectedItem = ssel.size() > 0 && ssel.size() < viewer.getTable().getItemCount();
    btnRemove.setEnabled( moreThanSelectedItem );
  }

  private void autoSelectSingle( final IStructuredSelection prev ) {
    IStructuredSelection curr = ( IStructuredSelection ) getSelection();
    if( !curr.equals( prev ) || curr.isEmpty() ) {
      if( curr.size() == 0 && impls.size() == 1 ) {
        // pick a default automatically
        setSelection( new StructuredSelection( impls.get( 0 ) ) );
      }
    }
    fireSelectionChanged();
  }

  private void addCabalImplementation() {
    IStructuredSelection prev = ( IStructuredSelection )getSelection();
    CabalImplementationDialog dialog = new CabalImplementationDialog( table.getShell(), null );
    dialog.setTitle( UITexts.cabalImplsBlock_dlgAdd );
    if( dialog.open() == Window.OK ) {
      CabalImplementation impl = dialog.getResult();
      if (validateImpl(impl)) {
        add( impl );
        viewer.setInput( impl );
        viewer.refresh();
      }
      autoSelectSingle( prev );
    }
  }

  private void editCabalImplementation() {
    IStructuredSelection prev = ( IStructuredSelection )getSelection();
    int selected = table.getSelectionIndex();
    if (selected >= 0) {
      CabalImplementation impl = impls.get(selected);
      String implIdent = impl.getUserIdentifier();
      CabalImplementationDialog dialog = new CabalImplementationDialog( table.getShell(), impl );
      dialog.setTitle( UITexts.cabalImplsBlock_dlgEdit );
      if (dialog.open() == Window.OK) {
        CabalImplementation updatedImpl = dialog.getResult();
        if (validateImpl( updatedImpl )) {
          update( implIdent, updatedImpl );
          setCheckedCabalImplementation( updatedImpl.getUserIdentifier() );
          viewer.setInput( impl );
        }
        viewer.refresh();
        autoSelectSingle( prev );
      }
    }
  }

  private void removeSelectedCabalImplementations() {
    IStructuredSelection ssel = ( IStructuredSelection ) viewer.getSelection();
    IStructuredSelection prev = ( IStructuredSelection ) getSelection();
    CabalImplementation[] insts = new CabalImplementation[ ssel.size() ];
    Iterator iter = ssel.iterator();
    int i = 0;
    while( iter.hasNext() ) {
      insts[ i ] = ( CabalImplementation ) iter.next();
      i++;
    }
    removeCabalImplementations( insts );
    viewer.refresh();
    autoSelectSingle( prev );
  }

  private void removeCabalImplementations( final CabalImplementation[] insts) {
    for( int i = 0; i < insts.length; i++ ) {
      impls.remove( insts[ i ] );
    }
  }

  private void autoDetectCabalImpls() {
    ArrayList<File> candidateLocs = FileUtil.getCandidateLocations();

    viewer.remove( impls );
    viewer.setInput( null );
    impls.clear();

    for (File loc : candidateLocs) {
      File[] files = loc.listFiles( new FilenameFilter() {
        public boolean accept( final File dir, final String name ) {
          // Catch anything starting with "cabal", because MacPorts (and others) may install
          // "cabal-1.8.0" as a legitimate cabal executable.
          return name.startsWith( CabalImplementation.CABAL_BASENAME );
        }
      });

      if (files != null && files.length > 0) {
        for (File file : files) {
          try {
            CabalImplementation impl = new CabalImplementation("foo", new Path(file.getCanonicalPath()));

            int seqno = 1;
            String ident = CabalImplementation.CABAL_BASENAME.concat( "-" ).concat(impl.getInstallVersion());
            if (!isUniqueUserIdentifier( ident )) {
              String uniqIdent = ident.concat( "-" ).concat( String.valueOf( seqno ) );
              while (!isUniqueUserIdentifier(uniqIdent)) {
                seqno++;
                uniqIdent = ident.concat( "-" ).concat( String.valueOf( seqno ) );
              }
              ident = uniqIdent;
            }

            impl.setUserIdentifier( ident );
            impls.add( impl );
          } catch (IOException e) {
            // Ignore?
          }
        }
      }
    }

    if (impls.size() > 0) {
      viewer.add(impls);
      viewer.setInput( impls.toArray() );
      setSelection(new StructuredSelection( impls.get(0) ));
    } else {
      setSelection( new StructuredSelection ( ) );
    }

    viewer.refresh(true);
  }

  private boolean validateImpl( final CabalImplementation impl ) {
    return (impl != null) && isUniqueUserIdentifier(impl.getUserIdentifier());
  }

  private boolean isUniqueUserIdentifier (final String ident) {
    boolean retval = true;
    for (CabalImplementation impl : impls) {
      if (impl.getUserIdentifier().equals(ident)) {
        retval = false;
        break;
      }
    }
    return retval;
  }

  private void sortByUserIdentifier() {
    viewer.setComparator( new Compare_UserIdentifier() );
    sortColumn = 1;
  }

  private void sortByExecutablePath() {
    viewer.setComparator( new Compare_ExecutablePath() );
    sortColumn = 4;
  }

  private void sortByInstallVersion() {
    viewer.setComparator( new Compare_InstallVersion() );
    sortColumn = 2;
  }

  private void sortByLibraryVersion() {
    viewer.setComparator( new Compare_LibraryVersion() );
    sortColumn = 3;
  }

  public void saveColumnSettings( final IDialogSettings settings, final String qualifier ) {
    int columnCount = table.getColumnCount();
    for( int i = 0; i < columnCount; i++ ) {
      int width = table.getColumn( i ).getWidth();
      settings.put( qualifier + KEY_COLUMN_WIDTH + i, width );
    }
    settings.put( qualifier + KEY_SORT_COLUMN, sortColumn );
  }

  public void restoreColumnSettings( final IDialogSettings settings, final String qualifier ) {
    viewer.getTable().layout( true );
    restoreColumnWidths( settings, qualifier );
    try {
      sortColumn = settings.getInt( qualifier + KEY_SORT_COLUMN );
    } catch( final NumberFormatException numfex ) {
      sortColumn = 1;
    }
    switch( sortColumn ) {
      case 1:
        sortByUserIdentifier();
        break;
      case 2:
        sortByInstallVersion();
        break;
      case 3:
        sortByLibraryVersion();
        break;
      case 4:
        sortByExecutablePath();
        break;
    }
  }

  private void restoreColumnWidths( final IDialogSettings settings, final String qualifier ) {
    int columnCount = table.getColumnCount();
    for( int i = 0; i < columnCount; i++ ) {
      int width = -1;

      try {
        width = settings.getInt( qualifier + KEY_COLUMN_WIDTH + i );
      } catch( final NumberFormatException numfex ) {
        // ignored
      }

      if( width > 0 ) {
        // Only override column widths if the preference exists, otherwise,
        // we go with the column weights previously configured in the table's
        // layout.
        table.getColumn( i ).setWidth( width );
      }
    }
  }

  /** The internal content provider class */
  private class CabalImplsCP implements IStructuredContentProvider {
    CabalImplsCP( final List<CabalImplementation> impls ) {
      // Unused.
    }

    public void dispose() {
      // Unused.
    }

    public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
      // unused

    }

    public Object[] getElements( final Object inputElement ) {
      return impls.toArray();
    }
  }

  /** Internal table label provider class */
  public class CabalImplsLP extends LabelProvider implements ITableLabelProvider {

    public Image getColumnImage( final Object element, final int columnIndex ) {
      return null;
    }

    public String getColumnText( final Object elem, final int columnIndex ) {
      String result = null;
      if ( elem instanceof CabalImplementation ) {
          CabalImplementation impl = ( CabalImplementation ) elem;
          switch( columnIndex ) {
            case 0: {
              result = impl.getUserIdentifier();
              break;
            }
            case 1:
              result = impl.getInstallVersion();
              break;
            case 2:
              result = impl.getLibraryVersion();
              break;
            case 3: {
              result = impl.getCabalExecutableName().toOSString();
              break;
            }
          }
      } else {
        result = elem.toString();
      }

      return result;
    }
  }

  /** Internal viewer comparator class: sort by user identifier */
  private final class Compare_UserIdentifier extends ViewerComparator {
    @Override
    public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
      int result = super.compare( viewer, e1, e2 );
      if(    ( e1 instanceof CabalImplementation )
          && ( e2 instanceof CabalImplementation ) ) {
        CabalImplementation left = ( CabalImplementation ) e1;
        CabalImplementation right = ( CabalImplementation )e2;
        result = left.getUserIdentifier().compareToIgnoreCase( right.getUserIdentifier() );
      }
      return result;
    }

    @Override
    public boolean isSorterProperty( final Object element, final String property ) {
      return true;
    }
  }

  public class Compare_ExecutablePath extends ViewerComparator {
    @Override
    public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
      int result = super.compare( viewer, e1, e2 );
      if(    ( e1 instanceof CabalImplementation )
          && ( e2 instanceof CabalImplementation ) ) {
        CabalImplementation left = ( CabalImplementation ) e1;
        CabalImplementation right = ( CabalImplementation )e2;
        String pathLeft = left.getCabalExecutableName().toOSString();
        String pathRight = right.getCabalExecutableName().toOSString();
        result = pathLeft.compareToIgnoreCase( pathRight );
      }
      return result;
    }

    @Override
    public boolean isSorterProperty( final Object element, final String property ) {
      return true;
    }
  }

  public class Compare_InstallVersion extends ViewerComparator {
    @Override
    public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
      int result = super.compare( viewer, e1, e2 );
      if(    ( e1 instanceof CabalImplementation )
          && ( e2 instanceof CabalImplementation ) ) {
        CabalImplementation left = ( CabalImplementation ) e1;
        CabalImplementation right = ( CabalImplementation )e2;
        result = left.getInstallVersion().compareToIgnoreCase( right.getInstallVersion() );
      }
      return result;
    }

    @Override
    public boolean isSorterProperty( final Object element, final String property ) {
      return true;
    }
  }

  public class Compare_LibraryVersion extends ViewerComparator {
    @Override
    public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
      int result = super.compare( viewer, e1, e2 );
      if(    ( e1 instanceof CabalImplementation )
          && ( e2 instanceof CabalImplementation ) ) {
        CabalImplementation left = ( CabalImplementation ) e1;
        CabalImplementation right = ( CabalImplementation )e2;
        result = left.getLibraryVersion().compareToIgnoreCase( right.getLibraryVersion() );
      }
      return result;
    }

    @Override
    public boolean isSorterProperty( final Object element, final String property ) {
      return true;
    }
  }
}