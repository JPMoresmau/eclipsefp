//Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.util;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Control;


/** <p>contains some static helping functionality for dialog layout etc.</p>
  * 
  * @author Leif Frenzel
  */
public class DialogUtil {

  public static int convertHeightInCharsToPixels( final Control control, 
                                                  final int chars ) {
    int result = 0;
    FontMetrics fontMetrics = getFontMetrics( control );
    if( fontMetrics != null ) {
      result = Dialog.convertHeightInCharsToPixels( fontMetrics, chars );
    }
    return result;
  }

  public static int convertWidthInCharsToPixels( final Control control, 
                                                 final int chars ) {
    int result = 0;
    FontMetrics fontMetrics = getFontMetrics( control );
    if( fontMetrics != null ) {
      result = Dialog.convertWidthInCharsToPixels( fontMetrics, chars );
    }
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private static FontMetrics getFontMetrics( final Control control ) {
    GC gc = new GC( control );
    gc.setFont( control.getFont() );
    FontMetrics fontMetrics = gc.getFontMetrics();
    gc.dispose();
    return fontMetrics;
  }
}