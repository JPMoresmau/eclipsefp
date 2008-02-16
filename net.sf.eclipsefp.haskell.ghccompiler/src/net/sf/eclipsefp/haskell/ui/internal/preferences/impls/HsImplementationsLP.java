// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.impls;

import net.sf.eclipsefp.haskell.core.internal.hsimpl.IHsImplementation;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

class HsImplementationsLP extends LabelProvider implements ITableLabelProvider {

  // interface methods of ITableLabelProvider
  ///////////// //////////////////////////////

  public String getColumnText( final Object elem, final int column ) {
    String result = elem.toString();
    if( elem instanceof IHsImplementation ) {
        IHsImplementation impl
          = ( IHsImplementation ) elem;
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
    }
    return result;
  }

  public Image getColumnImage( final Object elem, final int column ) {
    return null;
  }
}