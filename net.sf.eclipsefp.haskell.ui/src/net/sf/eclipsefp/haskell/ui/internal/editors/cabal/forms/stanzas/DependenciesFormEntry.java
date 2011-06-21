package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
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
import org.eclipse.ui.forms.widgets.FormToolkit;


public class DependenciesFormEntry extends FormEntry {

  private Composite composite;
  private Table tableField;
  private Button addButton;
  private Button removeButton;

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    composite = toolkit.createComposite( parent );
    GridLayout layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.numColumns = 2;
    composite.setLayout( layout );

    tableField = toolkit.createTable( composite, SWT.SINGLE | SWT.FULL_SELECTION );
    GridData listGD = new GridData( GridData.FILL_BOTH );
    listGD.grabExcessHorizontalSpace = true;
    listGD.grabExcessVerticalSpace = true;
    listGD.verticalSpan = 2;
    tableField.setLayoutData( listGD );

    tableField.setHeaderVisible( true );
    TableColumn column1 = new TableColumn( tableField, SWT.NULL );
    column1.setText( "Package" );
    column1.pack();
    TableColumn column2 = new TableColumn( tableField, SWT.NULL );
    column2.setText( "Version" );
    column2.pack();

    Composite buttonComposite = toolkit.createComposite( composite );
    GridLayout buttonLayout = new GridLayout();
    buttonLayout.marginHeight = 0;
    buttonLayout.marginWidth = 0;
    buttonLayout.numColumns = 1;
    buttonLayout.makeColumnsEqualWidth = true;
    buttonComposite.setLayout( buttonLayout );

    addButton = toolkit.createButton( buttonComposite, UITexts.cabalEditor_add,
        SWT.NONE );
    addButton.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    addButton.addSelectionListener( new SelectionAdapter() {

      @Override
      public void widgetSelected( final SelectionEvent e ) {
        // Add
      }
    } );

    removeButton = toolkit.createButton( buttonComposite, UITexts.cabalEditor_remove,
        SWT.NONE );
    removeButton.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    removeButton.addSelectionListener( new SelectionAdapter() {

      @Override
      public void widgetSelected( final SelectionEvent e ) {
        // Remove
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
    /*String newValue = ( value == null ) ? "" : value;
    String oldValue = this.getValue();
    if( newValue.equals( oldValue ) ) {
      return;
    }

    compilerList.removeAll();
    Vector<String> elements = StringUtils.split( newValue, ',' );
    compilerList.removeAll();
    for( String element: elements ) {
      compilerList.add( element.trim() );
    }

    // If we didn't want to block notifications, notify at the end
    if( !blockNotification ) {
      notifyTextValueChanged();
    }*/
  }

  @Override
  public String getValue() {
    /*StringBuilder builder = new StringBuilder();
    for( String element: compilerList.getItems() ) {
      if( builder.length() > 0 ) {
        builder.append( ", " );
      }
      builder.append( element );
    }
    return builder.toString();*/
    return null;
  }

  @Override
  public void setEditable( final boolean editable ) {
    tableField.setEnabled( editable );
    addButton.setEnabled( editable );
    removeButton.setEnabled( editable );
  }

}
