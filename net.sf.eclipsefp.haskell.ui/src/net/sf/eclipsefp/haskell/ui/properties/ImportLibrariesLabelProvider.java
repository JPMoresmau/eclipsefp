// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/** <p>the label provider for ImportLibraries on the viewer.</p>
  *
  * @author Leif Frenzel
  */
class ImportLibrariesLabelProvider extends LabelProvider {

  // interface methods of LabelProvider
  /////////////////////////////////////

  @Override
  public Image getImage( final Object element ) {
    CabalPackage pkg = ( CabalPackage )element;
    String key=pkg.isExposed()?IImageNames.PACKAGE: IImageNames.HIDDEN_PACKAGE;
    return HaskellUIImages.getImage( key );
  }
}