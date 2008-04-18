// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.common;

import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/** <p>label provider for generic trees consisting of ITreeElements.</p>
  *
  * @author Leif Frenzel
  */
public class TreeElementLP extends LabelProvider {

  // interface methods of LabelProvider
  /////////////////////////////////////

  @Override
  public String getText( final Object element ) {
    String result;
    if( element instanceof ITreeElement ) {
      result = ( ( ITreeElement )element ).getText();
    } else {
      result = super.getText( element );
    }
    return result;
  }

  @Override
  public Image getImage( final Object element ) {
    Image result;
    if( element instanceof ITreeElement ) {
      String key = ( ( ITreeElement )element ).getImageKey();
      result = HaskellUIImages.getImage( key );
    } else {
      result = super.getImage( element );
    }
    return result;
  }
}
