// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;


/** Utility class to simplify access to some SWT resources.
  *
  * @author Leif Frenzel
  */
public class SWTUtil {

  /**
   * Returns a width hint for a button control.
   */
  public static int getButtonWidthHint( final Button button ) {
    if( button.getFont().equals( JFaceResources.getDefaultFont() ) ) {
      button.setFont( JFaceResources.getDialogFont() );
    }
    PixelConverter pc = new PixelConverter( button );
    int widthHint = pc.convertHorizontalDLUsToPixels( IDialogConstants.BUTTON_WIDTH );
    Point size = button.computeSize( SWT.DEFAULT, SWT.DEFAULT, true );
    return Math.max( widthHint, size.x );
  }

  /**
   * Sets width and height hint for the button control. <b>Note: </b> This is a
   * NOP if the button's layout data is not an instance of
   * <code>GridData</code>.
   *
   * @param the
   *          button for which to set the dimension hint
   */
  public static void setButtonDimensionHint( final Button button ) {
    Assert.isNotNull( button );
    Object gd = button.getLayoutData();
    if( gd instanceof GridData ) {
      ( ( GridData )gd ).widthHint = getButtonWidthHint( button );
    }
  }

  public static Button createPushButton( final Composite parent,
                                         final String text ) {
    Button button = new Button( parent, SWT.PUSH );
    button.setFont( parent.getFont() );
    if( text != null ) {
      button.setText( text );
    }

    GridData gridData = new GridData( SWT.FILL, SWT.TOP, true, false );
    gridData.widthHint = getButtonWidthHint( button );
    button.setLayoutData( gridData );
    return button;
  }

  /** Create a message label
   *
   * @param parent Parent composite into which this message label is inserted
   * @param text The text to display in the label
   * @param hspan The horizontal span, in columns, that this label will span
   * @param wrapwidth The width hint used for wrapping the text.
   */
  public static void createMessageLabel( final Composite parent,
                                    final String text,
                                    final int hspan,
                                    final int wrapwidth ) {
    Label label = new Label( parent, SWT.WRAP );
    label.setFont( parent.getFont() );
    label.setText( text );

    GridData gridData = new GridData( SWT.LEFT, SWT.TOP, false, false );
    gridData.horizontalSpan = hspan;
    gridData.widthHint = wrapwidth;
    label.setLayoutData( gridData );
  }

  /** Create an invisible line spacer
   *
   * @param parent The parent Composiste into which this space is inserted
   * @param lines The number of lines this spacer takes up.
   */
  public static void createLineSpacer( final Composite parent, final int lines ) {
    Label lbl = new Label( parent, SWT.NONE );

    GridData gridData = new GridData( SWT.LEFT, SWT.TOP, false, false );
    gridData.heightHint = lines;
    lbl.setLayoutData( gridData );
  }

  public static Composite createMainComposite( final Composite parent ) {
    Composite result = new Composite( parent, SWT.NONE );
    GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    result.setLayout( gridLayout );
    result.setFont( parent.getFont() );
    return result;
  }

  public static Table createTable( final Composite parent ) {
    int style = SWT.CHECK | SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION;
    Table table = new Table( parent, style );

    GridData gridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    gridData.widthHint = 450;
    table.setLayoutData( gridData );
    table.setFont( parent.getFont() );
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    return table;
  }

  public static Label createLabel( final Composite parent,
                                   final String text,
                                   final int hspan ) {
    Label result = new Label( parent, SWT.NONE );
    result.setFont( parent.getFont() );
    result.setText( text );
    GridData gd = new GridData( SWT.FILL, SWT.TOP, true, false );
    gd.horizontalSpan = hspan;
    result.setLayoutData( gd );
    return result;
  }

  public static Text createSingleText( final Composite parent,
                                       final int hspan ) {
    Text result = new Text( parent, SWT.SINGLE | SWT.BORDER );
    result.setFont( parent.getFont() );
    GridData gd = new GridData( GridData.FILL_HORIZONTAL );
    gd.horizontalSpan = hspan;
    result.setLayoutData( gd );
    return result;
  }
}