// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageRef;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/** <p>the label provider for ImportLibraries on the viewer.</p>
  *
  * @author Leif Frenzel
  */
public class ImportLibrariesLabelProvider extends LabelProvider {
  private boolean showInstalled=false;
  //private ImageDescriptor installedOverlay;
  private final Image installedPackage;
  private final Image installedHiddenPackage;

  // interface methods of LabelProvider
  /////////////////////////////////////

  public ImportLibrariesLabelProvider() {
    super();
    ImageDescriptor installedOverlay=HaskellUIImages.getImageDescriptor( IImageNames.SUCCESS_OVERLAY );
    installedPackage=new DecorationOverlayIcon( HaskellUIImages.getImage( IImageNames.PACKAGE ), installedOverlay , IDecoration.TOP_LEFT).createImage();
    installedHiddenPackage=new DecorationOverlayIcon( HaskellUIImages.getImage( IImageNames.HIDDEN_PACKAGE ), installedOverlay , IDecoration.TOP_LEFT).createImage();

  }

  @Override
  public Image getImage( final Object element ) {
    if (element instanceof CabalPackage){
      CabalPackage pkg = ( CabalPackage )element;
      String key=pkg.isExposed()?IImageNames.PACKAGE: IImageNames.HIDDEN_PACKAGE;
      return HaskellUIImages.getImage( key );
    } else if (element instanceof CabalPackageRef){
      CabalPackageRef ref=(CabalPackageRef)element;
      if (showInstalled && ref.isInstalled()){
        return installedPackage;
      }
      return HaskellUIImages.getImage( IImageNames.PACKAGE );
    } else if (element instanceof CabalPackageVersion){
      CabalPackageVersion v=(CabalPackageVersion)element;
      if (showInstalled && v.isInstalled()){
        return v.isLast()?installedPackage: installedHiddenPackage;
      }
      String key=v.isLast()?IImageNames.PACKAGE: IImageNames.HIDDEN_PACKAGE;
      return HaskellUIImages.getImage( key );
    }

    return HaskellUIImages.getImage( IImageNames.PACKAGE );
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.BaseLabelProvider#dispose()
   */
  @Override
  public void dispose() {
    installedPackage.dispose();
    installedHiddenPackage.dispose();
    super.dispose();
  }

  // specific methods

  public boolean isShowInstalled() {
    return showInstalled;
  }


  public void setShowInstalled( final boolean showInstalled ) {
    this.showInstalled = showInstalled;
  }
}