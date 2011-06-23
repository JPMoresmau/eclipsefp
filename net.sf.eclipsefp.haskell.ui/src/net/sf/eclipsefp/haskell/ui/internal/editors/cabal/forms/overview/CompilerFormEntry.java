package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import java.util.Vector;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.apache.tools.ant.util.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class CompilerFormEntry extends FormEntry {

  private Composite composite;
  private List compilerList;
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

    compilerList = new List( composite, SWT.SINGLE | SWT.BORDER );
    toolkit.adapt( compilerList, true, true );
    GridData listGD = new GridData( GridData.FILL_BOTH );
    listGD.grabExcessHorizontalSpace = true;
    listGD.grabExcessVerticalSpace = true;
    listGD.verticalSpan = 2;
    compilerList.setLayoutData( listGD );

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
        ChooseCompilerDialog dialog = new ChooseCompilerDialog( composite
            .getShell() );
        if( dialog.open() == Window.OK ) {
          compilerList.add( dialog.getValue() );
          CompilerFormEntry.this.notifyTextValueChanged();
        }
      }
    } );

    removeButton = toolkit.createButton( buttonComposite, UITexts.cabalEditor_remove,
        SWT.NONE );
    removeButton.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    removeButton.addSelectionListener( new SelectionAdapter() {

      @Override
      public void widgetSelected( final SelectionEvent e ) {
        if( compilerList.getSelectionIndices().length > 0 ) {
          compilerList.remove( compilerList.getSelectionIndices() );
          CompilerFormEntry.this.notifyTextValueChanged();
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
    return 120;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    String newValue = ( value == null ) ? "" : value;
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
    }
  }

  @Override
  public String getValue() {
    StringBuilder builder = new StringBuilder();
    for( String element: compilerList.getItems() ) {
      if( builder.length() > 0 ) {
        builder.append( ", " );
      }
      builder.append( element );
    }
    return builder.toString();
  }

  @Override
  public void setEditable( final boolean editable ) {
    compilerList.setEnabled( editable );
    addButton.setEnabled( editable );
    removeButton.setEnabled( editable );
  }

}
