/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.Iterator;
import java.util.Vector;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.apache.tools.ant.util.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class DependenciesFormEntry extends FormEntry implements ICellModifier {

  private Composite composite;
  private TableViewer tableField;
  private Button addButton;
  private Button removeButton;
  private Vector<DependencyItem> items;
  private boolean isCellEditing = false;

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    composite = toolkit.createComposite( parent );
    GridLayout layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.numColumns = 2;
    composite.setLayout( layout );

    items = new Vector<DependencyItem>();

    Table table = toolkit.createTable( composite, SWT.SINGLE );
    GridData listGD = new GridData( GridData.FILL_BOTH );
    listGD.grabExcessHorizontalSpace = true;
    listGD.grabExcessVerticalSpace = true;
    listGD.verticalSpan = 2;
    table.setLayoutData( listGD );
    table.setHeaderVisible( true );
    TableColumn column1 = new TableColumn( table, SWT.NULL );
    column1.setText( UITexts.cabalEditor_dependenciesPackage );
    column1.setWidth( 150 );
    TableColumn column2 = new TableColumn( table, SWT.NULL );
    column2.setText( UITexts.cabalEditor_dependenciesVersion );
    column2.pack();

    tableField = new TableViewer( table );
    tableField.setUseHashlookup( true );
    tableField.setColumnProperties( new String[] { "package", "version" } );

    tableField.setCellEditors( new CellEditor[] { null,
        new TextCellEditor( table ) } );
    tableField.setCellModifier( this );

    tableField.setLabelProvider( new DependenciesTableLabelProvider() );
    tableField.setContentProvider( new DependenciesTableContentProvider() );
    tableField.setInput( items );

    Composite buttonComposite = toolkit.createComposite( composite );
    GridLayout buttonLayout = new GridLayout();
    buttonLayout.marginHeight = 0;
    buttonLayout.marginWidth = 0;
    buttonLayout.numColumns = 1;
    buttonLayout.makeColumnsEqualWidth = true;
    buttonComposite.setLayout( buttonLayout );

    addButton = toolkit.createButton( buttonComposite, UITexts.cabalEditor_add,
        SWT.NONE );
    addButton.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    addButton.addSelectionListener( new SelectionAdapter() {

      @Override
      public void widgetSelected( final SelectionEvent e ) {
        Vector<String> alreadySelected = new Vector<String>();
        for( DependencyItem item: items ) {
          alreadySelected.add( item.getPackage() );
        }
        DependenciesDialog dialog = new DependenciesDialog( composite
            .getShell(), alreadySelected );
        if( dialog.open() == Window.OK && dialog.getValue() != null ) {
          DependencyItem item = new DependencyItem( dialog.getValue(), "" );
          items.add( item );
          tableField.setInput( items );
          DependenciesFormEntry.this.notifyTextValueChanged();
        }
      }
    } );

    removeButton = toolkit.createButton( buttonComposite,
        UITexts.cabalEditor_remove, SWT.NONE );
    removeButton.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    removeButton.addSelectionListener( new SelectionAdapter() {

      @Override
      public void widgetSelected( final SelectionEvent e ) {
        // Remove the selected elements
        IStructuredSelection selection = ( IStructuredSelection )tableField
            .getSelection();
        if( !selection.isEmpty() ) {
          Iterator<DependencyItem> iterator = selection.iterator();
          while( iterator.hasNext() ) {
            items.remove( iterator.next() );
          }
          tableField.setInput( items );
          DependenciesFormEntry.this.notifyTextValueChanged();
        }
      }
    } );
  }

  @Override
  public Control getControl() {
    return composite;
  }

  @Override
  public int heightHint() {
    return 200;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {

    if (isCellEditing) {
      return;
    }

    String newValue = (( value == null ) ? "" : value).trim();
    String oldValue = this.getValue().trim();
    if( newValue.equals( oldValue ) ) {
      return;
    }

    isCellEditing = true;
    Vector<String> elements = StringUtils.split( newValue, ',' );
    items = new Vector<DependencyItem>();
    for( String element: elements ) {
      items.add( DependencyItem.fromString( element ) );
    }
    tableField.setInput( items );

    // If we didn't want to block notifications, notify at the end
    if( !blockNotification ) {
      notifyTextValueChanged();
    }
    isCellEditing = false;
  }

  @Override
  public String getValue() {
    StringBuilder builder = new StringBuilder();
    for( DependencyItem item: items ) {
      if( builder.length() > 0 ) {
        builder.append( ", " );
      }
      builder.append( item.getPackage() );
      if (!item.getVersion().trim().isEmpty()) {
        builder.append( ' ' );
        builder.append( item.getVersion() );
      }
    }
    return builder.toString();
  }

  @Override
  public void setEditable( final boolean editable ) {
    tableField.getControl().setEnabled( editable );
    addButton.setEnabled( editable );
    removeButton.setEnabled( editable );
  }

  public boolean canModify( final Object element, final String property ) {
    return ( property.equals( "version" ) );
  }

  public Object getValue( final Object element, final String property ) {
    DependencyItem item = ( DependencyItem )element;
    if( property.equals( "package" ) ) {
      return item.getPackage();
    } else if( property.equals( "version" ) ) {
      return item.getVersion();
    } else {
      return null;
    }
  }

  public void modify( final Object element, final String property,
      final Object value ) {
    if (element == null) {
      return;
    }

    isCellEditing = true;
    DependencyItem item = ( DependencyItem )( ( TableItem )element ).getData();
    if( property.equals( "version" ) ) {
      String newValue = ( String )value;
      if( !item.getVersion().equals( newValue ) ) {
        item.setVersion( newValue );
        tableField.setInput( items );
        notifyTextValueChanged();
      }
    }
    isCellEditing = false;
  }

}
