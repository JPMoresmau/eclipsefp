package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * A field editor that consists of a push button. Unlike other field editors, it
 * does not actually store a value, but it can be used to modify values in
 * another field.
 *
 * @author Thomas ten Cate
 */
public class ButtonFieldEditor extends FieldEditor {

  private String buttonText;

  private final SelectionListener buttonListener;

  private Button button;

  public ButtonFieldEditor( final String labelText, final String buttonText,
      final SelectionListener buttonListener, final Composite parent ) {
    this.buttonText = buttonText;
    this.buttonListener = buttonListener;
    setLabelText( labelText );
    createControl( parent );
  }

  @Override
  public int getNumberOfControls() {
    return 2;
  }

  @Override
  protected void doFillIntoGrid( final Composite parent, final int numColumns ) {
    // Label label = getLabelControl( parent );
    // GridData layoutData = new GridData( );
    // layoutData.horizontalSpan = numColumns - 1;
    // layoutData.grabExcessHorizontalSpace = true;
    // label.setLayoutData( layoutData );

    // Button button = getButtonControl( parent );
    // setButtonLayoutData( button );

    getLabelControl(parent);
    getButtonControl(parent);
    adjustForNumColumns( numColumns );
  }

  @Override
  protected void adjustForNumColumns( final int numColumns ) {
    // keep the button right-aligned

    Label label = getLabelControl();
    GridData layoutData = new GridData();
    layoutData.horizontalAlignment = SWT.FILL;
    layoutData.horizontalSpan = numColumns - 1;
    label.setLayoutData( layoutData );

    layoutData = new GridData();
    button.setLayoutData( layoutData );
    // ( ( GridData )getLabelControl().getLayoutData() ).horizontalSpan = numColumns - 1;
  }

  protected int getButtonStyle(){
    return SWT.PUSH;
  }

  /**
   * Get the button control. Create it in parent if required.
   */
  protected Button getButtonControl( final Composite parent ) {
    if( button == null ) {
      button = new Button( parent, getButtonStyle() );
      if( buttonText == null ) {
        buttonText = JFaceResources.getString( "openChange" ); //$NON-NLS-1$
      }
      button.setText( buttonText );
      button.setFont( parent.getFont() );
      button.addSelectionListener( buttonListener );
      button.addDisposeListener( new DisposeListener() {

        public void widgetDisposed( final DisposeEvent event ) {
          button = null;
        }
      } );
    } else {
      checkParent( button, parent );
    }
    return button;
  }

  @Override
  public void setEnabled( final boolean enabled, final Composite parent ) {
    super.setEnabled( enabled, parent );
    getButtonControl( parent ).setEnabled( enabled );
  }

  @Override
  protected void doLoad() {
    // does nothing
  }

  @Override
  protected void doLoadDefault() {
    // does nothing
  }

  @Override
  protected void doStore() {
    // does nothing
  }

}
