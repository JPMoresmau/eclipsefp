// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.util;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;


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
    PixelConverter converter = new PixelConverter( button );
    int widthHint = converter
        .convertHorizontalDLUsToPixels( IDialogConstants.BUTTON_WIDTH );
    return Math.max( widthHint, button.computeSize( SWT.DEFAULT, SWT.DEFAULT,
        true ).x );
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
}