// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors.outline;

import net.sf.eclipsefp.haskell.cabal.core.model.ExecutableStanza;
import net.sf.eclipsefp.haskell.cabal.core.model.GeneralStanza;
import net.sf.eclipsefp.haskell.cabal.core.model.LibraryStanza;
import net.sf.eclipsefp.haskell.cabal.ui.internal.util.CabalUIImages;
import net.sf.eclipsefp.haskell.cabal.ui.internal.util.IImageNames;

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
      result = CabalUIImages.getImage( IImageNames.EXECUTABLE_STANZA );
    } else if( element instanceof LibraryStanza ) {
      result = CabalUIImages.getImage( IImageNames.LIBRARY_STANZA );
    } else if( element instanceof GeneralStanza ) {
      result = CabalUIImages.getImage( IImageNames.GENERAL_STANZA );
    }
    return result;
  }
}
