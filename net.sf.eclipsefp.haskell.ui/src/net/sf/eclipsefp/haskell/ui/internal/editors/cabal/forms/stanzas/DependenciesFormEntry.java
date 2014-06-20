/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * Form entry for selecting the package dependencies of a stanza.
 * @author Alejandro Serrano
 *
 */
public class DependenciesFormEntry extends FormEntry implements ICellModifier {

  private TableViewer tableField;
  private Action addAction;
  private Action removeAction;
  private List<DependencyItem> items;
  private boolean isCellEditing = false;

  private boolean denySelfRef=true;

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {

    items = new Vector<>();

    Table table = toolkit.createTable( parent, SWT.MULTI );
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

    addAction = new Action( UITexts.cabalEditor_add, IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        List<String> alreadySelected = new ArrayList<>();
        for( DependencyItem item: items ) {
          alreadySelected.add( item.getPackage() );
        }
        if (DependenciesFormEntry.this.denySelfRef){
          alreadySelected.add( project.getName() );
        }
        DependenciesDialog dialog = new DependenciesDialog( tableField
            .getTable().getShell(), alreadySelected,project.getName() );
        if( dialog.open() == Window.OK && dialog.getItems().size()>0 ) {
          items.addAll( dialog.getItems() );
          tableField.setInput( items );
          DependenciesFormEntry.this.notifyTextValueChanged();

        }
      }
    };
    addAction.setImageDescriptor( PlatformUI.getWorkbench().getSharedImages()
        .getImageDescriptor( ISharedImages.IMG_OBJ_ADD ) );

    removeAction = new Action( UITexts.cabalEditor_remove,
        IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        // Remove the selected elements
        IStructuredSelection selection = ( IStructuredSelection )tableField
            .getSelection();
        if( !selection.isEmpty() ) {
          Iterator<?> iterator = selection.iterator();
          while( iterator.hasNext() ) {
            items.remove( iterator.next() );
          }
          tableField.setInput( items );
          DependenciesFormEntry.this.notifyTextValueChanged();
        }
      }
    };
    removeAction.setImageDescriptor( PlatformUI.getWorkbench()
        .getSharedImages().getImageDescriptor( ISharedImages.IMG_TOOL_DELETE ) );
  }

  @Override
  public Control getControl() {
    return tableField.getTable();
  }

  @Override
  public int heightHint() {
    return 170;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {

    if( isCellEditing ) {
      return;
    }

    String newValue = ( ( value == null ) ? "" : value ).trim();
    String oldValue = this.getValue().trim();
    if( newValue.equals( oldValue ) ) {
      return;
    }

    isCellEditing = true;
    String[] elements = newValue.split( "," );
    items = new ArrayList<>(elements.length);
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
        builder.append( ","+PlatformUtil.NL );
      }
      builder.append( item.getPackage() );
      if( item.getVersion().trim().length() > 0 ) {
        builder.append( ' ' );
        builder.append( item.getVersion() );
      }
    }
    return builder.toString();
  }

  @Override
  public void setEditable( final boolean editable ) {
    tableField.getControl().setEnabled( editable );
  }

  @Override
  public boolean canModify( final Object element, final String property ) {
    return ( property.equals( "version" ) );
  }

  @Override
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

  @Override
  public void modify( final Object element, final String property,
      final Object value ) {
    if( element == null ) {
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

  public Action getAddAction() {
    return addAction;
  }

  public Action getRemoveAction() {
    return removeAction;
  }


  public boolean isDenySelfRef() {
    return denySelfRef;
  }


  public void setDenySelfRef( final boolean denySelfRef ) {
    this.denySelfRef = denySelfRef;
  }

}
