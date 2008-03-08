// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.outline;

import net.sf.eclipsefp.haskell.core.cabalmodel.ExecutableStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.GeneralStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.LibraryStanza;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/** <p>label provider for the Cabal editor's outline page.</p>
  *
  * @author Leif Frenzel
  */
class CabalOutlineLP extends LabelProvider {

  @Override
  public Image getImage( final Object element ) {
    Image result = super.getImage( element );
    if( element instanceof ExecutableStanza ) {
      result = HaskellUIImages.getImage( IImageNames.EXECUTABLE_STANZA );
    } else if( element instanceof LibraryStanza ) {
      result = HaskellUIImages.getImage( IImageNames.LIBRARY_STANZA );
    } else if( element instanceof GeneralStanza ) {
      result = HaskellUIImages.getImage( IImageNames.GENERAL_STANZA );
    }
    return result;
  }
}
