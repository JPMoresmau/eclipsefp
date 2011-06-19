package net.sf.eclipsefp.haskell.browser.util;

import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.swt.graphics.Image;

public class ImageCache {

  public static Image MODULE = HaskellUIImages.getImage( IImageNames.MODULE );
  public static Image MODULE_CONTENT = HaskellUIImages.getImage( IImageNames.EXPORT_MODULE_CONTENT );
  public static Image PACKAGE = HaskellUIImages.getImage( IImageNames.PACKAGE );
  public static Image DATABASE = HaskellUIImages.getImage( IImageNames.PACKAGE_FOLDER );
  public static Image DATATYPE = HaskellUIImages.getImage( IImageNames.DATA_DECL );
  public static Image CONSTRUCTOR = HaskellUIImages.getImage( IImageNames.CONSTRUCTOR_DECL );
  public static Image CLASS = HaskellUIImages.getImage( IImageNames.CLASS_DECL );
  public static Image INSTANCE = HaskellUIImages.getImage( IImageNames.INSTANCE_DECL );
  public static Image FUNCTION = HaskellUIImages.getImage( IImageNames.FUNCTION_BINDING );
  public static Image TYPE = HaskellUIImages.getImage( IImageNames.TYPE_DECL );

  public static Image getImageForDeclaration( final DeclarationType type ) {
    switch( type ) {
      case DATA_TYPE:
      case NEW_TYPE:
        return ImageCache.DATATYPE;
      case TYPE_CLASS:
        return ImageCache.CLASS;
      case INSTANCE:
        return ImageCache.INSTANCE;
      case FUNCTION:
        return ImageCache.FUNCTION;
      case TYPE_SYNONYM:
        return ImageCache.TYPE;
      default:
        return null;
    }
  }
}
