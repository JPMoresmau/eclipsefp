// Copyright (c) 2010 B. Scott Michel
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;


/** Table label provider for the Cabal implementations preference page.
 *
 * @author B. Scott Michel (scottm@aero.org)
 */
public class CabalImplsLP extends LabelProvider implements ITableLabelProvider {

  public Image getColumnImage( final Object element, final int columnIndex ) {
    return null;
  }

  public String getColumnText( final Object elem, final int columnIndex ) {
    String result = null;
    if ( elem instanceof CabalImplementation ) {
        CabalImplementation impl = ( CabalImplementation ) elem;
        switch( columnIndex ) {
          case 0:
            result = "--name--";
            break;
          case 1:
            result = "--version--";
            break;
          case 2:
            result = "--column3--";
            break;
        }
    } else {
      result = elem.toString();
    }

    return result;
  }

}
