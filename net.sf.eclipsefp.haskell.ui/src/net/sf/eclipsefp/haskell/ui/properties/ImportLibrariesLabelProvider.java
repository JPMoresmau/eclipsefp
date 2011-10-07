// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageRef;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/** <p>the label provider for ImportLibraries on the viewer.</p>
  *
  * @author Leif Frenzel
  */
public class ImportLibrariesLabelProvider extends LabelProvider {

  // interface methods of LabelProvider
  /////////////////////////////////////

  @Override
  public Image getImage( final Object element ) {
    if (element instanceof CabalPackage){
      CabalPackage pkg = ( CabalPackage )element;
      String key=pkg.isExposed()?IImageNames.PACKAGE: IImageNames.HIDDEN_PACKAGE;
      return HaskellUIImages.getImage( key );
    } else if (element instanceof CabalPackageRef){
      return HaskellUIImages.getImage( IImageNames.PACKAGE );
    } else if (element instanceof CabalPackageVersion){
      CabalPackageVersion v=(CabalPackageVersion)element;
      String key=v.isLast()?IImageNames.PACKAGE: IImageNames.HIDDEN_PACKAGE;
      return HaskellUIImages.getImage( key );
    }

    return HaskellUIImages.getImage( IImageNames.PACKAGE );
  }
}