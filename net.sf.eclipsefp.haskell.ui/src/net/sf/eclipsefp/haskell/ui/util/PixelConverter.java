// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Control;

/** <p>converter between pixels and dialog units.</p>
 *
 * @author Leif Frenzel
 */
public class PixelConverter {

  private final FontMetrics fFontMetrics;

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