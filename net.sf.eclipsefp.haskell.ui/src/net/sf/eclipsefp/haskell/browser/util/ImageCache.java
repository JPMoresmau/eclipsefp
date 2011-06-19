package net.sf.eclipsefp.haskell.browser.util;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import org.eclipse.swt.graphics.Image;

public class ImageCache {

  public static Image MODULE = BrowserPlugin.getImageDescriptor(
      "icons/obj16/module.gif" ).createImage();
  public static Image PACKAGE = BrowserPlugin.getImageDescriptor(
      "icons/obj16/package.gif" ).createImage();
  public static Image DATABASE = BrowserPlugin.getImageDescriptor(
      "icons/obj16/packagefolder.gif" ).createImage();
  public static Image DATATYPE = BrowserPlugin.getImageDescriptor(
      "icons/obj16/datadecl.gif" ).createImage();
  public static Image CONSTRUCTOR = BrowserPlugin.getImageDescriptor(
      "icons/obj16/constructordecl.gif" ).createImage();
  public static Image CLASS = BrowserPlugin.getImageDescriptor(
      "icons/obj16/classdecl.gif" ).createImage();
  public static Image INSTANCE = BrowserPlugin.getImageDescriptor(
      "icons/obj16/instancedecl.gif" ).createImage();
  public static Image FUNCTION = BrowserPlugin.getImageDescriptor(
      "icons/obj16/functionbinding.gif" ).createImage();
  public static Image TYPE = BrowserPlugin.getImageDescriptor(
      "icons/obj16/typedecl.gif" ).createImage();

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
