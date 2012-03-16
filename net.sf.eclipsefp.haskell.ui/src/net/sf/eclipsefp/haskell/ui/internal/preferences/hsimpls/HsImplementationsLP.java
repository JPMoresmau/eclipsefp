// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.hsimpls;

import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

class HsImplementationsLP extends LabelProvider implements ITableLabelProvider {

  // interface methods of ITableLabelProvider
  ///////////// //////////////////////////////

  @Override
  public String getColumnText( final Object elem, final int column ) {
    String result = null;
    if( elem instanceof IHsImplementation ) {
        IHsImplementation impl = ( IHsImplementation ) elem;
        switch( column ) {
          case 0:
            result = impl.getName();
            break;
          case 1:
            result = impl.getType().toString();
            break;
          case 2:
            result = impl.getVersion();
            break;
        }
    } else {
      result = elem.toString();
    }
    return result;
  }

  @Override
  public Image getColumnImage( final Object elem, final int column ) {
    return null;
  }
}