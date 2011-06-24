/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.forms.IFormColors;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.TableWrapData;


public class FormEntryDecorator extends FormEntry {

  private final String labelText;
  private Label label;
  private final boolean showAbove;
  private final FormEntry inner;
  private final int colspan;

  public static final int F_DEFAULT_TEXT_WIDTH_HINT = 100;

  public FormEntryDecorator( final String labelText, final FormEntry inner ) {
    this( labelText, false, inner, 1 );
  }

  public FormEntryDecorator( final String labelText, final FormEntry inner,
      final int colspan ) {
    this( labelText, false, inner, colspan );
  }

  public FormEntryDecorator( final String labelText, final boolean showAbove,
      final FormEntry inner ) {
    this( labelText, showAbove, inner, 1 );
  }

  public FormEntryDecorator( final String labelText, final boolean showAbove,
      final FormEntry inner, final int colspan ) {
    this.labelText = labelText;
    this.showAbove = showAbove;
    this.inner = inner;
    this.colspan = colspan;
  }

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    // Create description label
    label = toolkit.createLabel( parent, labelText );
    label.setForeground( toolkit.getColors().getColor( IFormColors.TITLE ) );
    // Initialize inner element
    inner.init( project, parent, toolkit, style );
    // Snap to grid
    Layout layout = parent.getLayout();
    if( layout instanceof GridLayout ) {
      snapToGridLayout( ( GridLayout )layout );
    }
    // Set the default text width hint and let clients modify accordingly
    // after the fact
    setWidthHint( F_DEFAULT_TEXT_WIDTH_HINT );
  }

  private void snapToGridLayout( final GridLayout layout ) {
    int numColumns = layout.numColumns;

    if (showAbove) {
      // Make both widgets fit the entire space
      GridData labelGd = new GridData(GridData.VERTICAL_ALIGN_CENTER);
      labelGd.horizontalSpan = numColumns;
      labelGd.grabExcessHorizontalSpace = true;
      label.setLayoutData( labelGd );
      GridData innerGd = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
      innerGd.horizontalSpan = numColumns;
      innerGd.grabExcessHorizontalSpace = true;
      innerGd.heightHint = inner.heightHint();
      inner.getControl().setLayoutData( innerGd );
    } else {
      // Make the widgets fit in (colspan, numColumns - colspan)
      GridData labelGd = new GridData(GridData.VERTICAL_ALIGN_CENTER);
      labelGd.horizontalSpan = this.colspan;
      label.setLayoutData( labelGd );
      GridData innerGd = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
      innerGd.horizontalSpan = numColumns - this.colspan;
      innerGd.grabExcessHorizontalSpace = true;
      innerGd.heightHint = inner.heightHint();
      inner.getControl().setLayoutData( innerGd );
    }
  }

  /**
   * If GridData was used, set the width hint. If TableWrapData was used set the
   * max width. If no layout data was specified, this method does nothing.
   */
  public void setWidthHint( final int width ) {
    Object data = inner.getControl().getLayoutData();
    if( data instanceof GridData ) {
      ( ( GridData )data ).widthHint = width;
    } else if( data instanceof TableWrapData ) {
      ( ( TableWrapData )data ).maxWidth = width;
    }
  }

  @Override
  public Control getControl() {
    return this.inner.getControl();
  }

  @Override
  public int heightHint() {
    return ( showAbove ? 10 : 0 ) + this.inner.heightHint();
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    this.inner.setValue( value, blockNotification );
  }

  @Override
  public String getValue() {
    return this.inner.getValue();
  }

  @Override
  public void setEditable( final boolean editable ) {
    this.inner.setEditable( editable );
  }

  @Override
  public void setProperty( final CabalSyntax property ) {
    this.inner.setProperty( property );
  }

  @Override
  public CabalSyntax getProperty() {
    return this.inner.getProperty();
  }

  @Override
  public void addFormEntryListener( final IFormEntryListener listener ) {
    // Listeners are routed to the inner item
    this.inner.addFormEntryListener( listener );
  }

  @Override
  public void removeFormEntryListener( final IFormEntryListener listener ) {
    // Listeners are routed to the inner item
    this.removeFormEntryListener( listener );
  }
}
