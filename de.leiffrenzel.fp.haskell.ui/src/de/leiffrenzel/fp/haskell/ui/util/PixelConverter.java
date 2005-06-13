// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.util;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Control;

/** <p>TODO</p>
 * 
 * @author Leif Frenzel
 */
public class PixelConverter {

  // TODO completely taken from org.eclipse.jdt.internal.ui.util
  // the same thing is in org.eclipse.pde.internal.ui.util
  // is there something like this in public API in 3.0??
  
  private FontMetrics fFontMetrics;
  
  public PixelConverter( final Control control ) {
    GC gc = new GC( control );
    gc.setFont( control.getFont() );
    fFontMetrics = gc.getFontMetrics();
    gc.dispose();
  }
    
  public int convertHeightInCharsToPixels( final int chars ) {
    return Dialog.convertHeightInCharsToPixels( fFontMetrics, chars );
  }

  public int convertHorizontalDLUsToPixels( final int dlus ) {
    return Dialog.convertHorizontalDLUsToPixels( fFontMetrics, dlus );
  }

  public int convertVerticalDLUsToPixels( final int dlus ) {
    return Dialog.convertVerticalDLUsToPixels( fFontMetrics, dlus );
  }
  
  public int convertWidthInCharsToPixels( final int chars ) {
    return Dialog.convertWidthInCharsToPixels( fFontMetrics, chars );
  } 
}