// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.layout.TableColumnLayout;
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


/** <p>TODO</p>
 *
 * @author Leif Frenzel
 */
public class CabalImplsBlock implements ISelectionProvider {
  private CheckboxTableViewer viewer;
  private Table table;
  private Button btnAdd;
  private Button btnRemove;
  private Button btnEdit;
  private final List<CabalImplementation> impls = new ArrayList<CabalImplementation>();
  private final ListenerList selectionListeners = new ListenerList();
  private ISelection lastSelection = new StructuredSelection();

  Composite createControl( final Composite parent ) {
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

  private void add(final CabalImplementation impl) {
    impls.add( impl );
    viewer.setInput( impl );
    viewer.refresh();
  }

  public void addSelectionChangedListener( final ISelectionChangedListener listener ) {
    selectionListeners.add( listener );
  }

  public ISelection getSelection() {
    return new StructuredSelection( viewer.getCheckedElements() );
  }

  public void removeSelectionChangedListener( final ISelectionChangedListener listener ) {
    selectionListeners.remove( listener );
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

  // Helper methods:
  private void createColumns(final Composite composite) {
    TableColumn colName = createColumn( UITexts.cabalImplsBlock_colName, new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        // sortByName();
      }
    } );
    TableColumn colInstallVErsion = createColumn (UITexts.cabalImplsBlock_colCabalInstallVersion, new SelectionAdapter() {
      @Override
      public void widgetSelected ( final SelectionEvent evt ) {
        // Insert something here.
      }
    } );
    TableColumn colLibraryVersion = createColumn (UITexts.cabalImplsBlock_colCabalLibraryVersion, new SelectionAdapter() {
      @Override
      public void widgetSelected ( final SelectionEvent evt ) {
        // Insert something here.
      }
    } );

    TableColumnLayout tcLayout = new TableColumnLayout();
    composite.setLayout( tcLayout );

    tcLayout.setColumnData( colName, new ColumnWeightData( 50, true ) );
    tcLayout.setColumnData( colInstallVErsion, new ColumnWeightData( 25, true ) );
    tcLayout.setColumnData( colLibraryVersion, new ColumnWeightData( 25, true ) );
  }

  private TableColumn createColumn( final String text,
      final SelectionListener listener ) {
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
        // TODO: Edit existing cabal implementation
      }
    } );

    String sRemove = UITexts.cabalImplsBlock_btnRemove;
    btnRemove = SWTUtil.createPushButton( buttonsComp, sRemove );
    btnRemove.addListener( SWT.Selection, new Listener() {
      public void handleEvent( final Event evt ) {
        // TODO: remove selected cabal implementation
      }
    } );
  }

  private void createViewer() {
    viewer = new CheckboxTableViewer( table );
    viewer.setLabelProvider( new CabalImplsLP() );
    viewer.setContentProvider( new CabalImplsCP( impls ) );
    // by default, sort by name
    // sortByName();

    viewer.addSelectionChangedListener( new ISelectionChangedListener() {
      public void selectionChanged( final SelectionChangedEvent evt ) {
        enableButtons();
      }
    } );

    viewer.addCheckStateListener( new ICheckStateListener() {
      public void checkStateChanged( final CheckStateChangedEvent event ) {
        if( event.getChecked() ) {
          CabalImplementation element = ( CabalImplementation )event.getElement();
          // TODO: setCheckedHsImplementation( element.getName() );
        } else {
          // TODO: setCheckedHsImplementation( null );
        }
      }
    } );

    viewer.addDoubleClickListener( new IDoubleClickListener() {
      public void doubleClick( final DoubleClickEvent e ) {
        if( !viewer.getSelection().isEmpty() ) {
          // TODO: editHsImplementation();
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
    IStructuredSelection curr = ( IStructuredSelection )getSelection();
    if( !curr.equals( prev ) || curr.isEmpty() ) {
      if( curr.size() == 0 && impls.size() == 1 ) {
        // pick a default automatically
        setSelection( new StructuredSelection( impls.get( 0 ) ) );
      }
    } else {
      fireSelectionChanged();
    }
  }

  private void addCabalImplementation() {
    IStructuredSelection prev = ( IStructuredSelection )getSelection();
    CabalImplementationDialog dialog = new CabalImplementationDialog( table.getShell(), this, null );
    dialog.setTitle( UITexts.cabalImplsBlock_dlgAdd );
    if( dialog.open() == Window.OK ) {
      add( dialog.getResult() );
      autoSelectSingle( prev );
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
            case 0:
              result = "--name--";
              break;
            case 1:
              result = "--version--";
              break;
            case 2:
              result = "--column3--";
              break;
          }
      } else {
        result = elem.toString();
      }

      return result;
    }

  }
}