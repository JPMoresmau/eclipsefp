// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.jface.resource.CompositeImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.osgi.framework.Bundle;

/** <p>contains the image registry for images in the Haskell Ui plugin.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellUIImages implements IImageNames {

  private static ImageRegistry imageRegistry;

  private static ImageDescriptorRegistry imageDescriptorRegistry;

  /** <p>a table of all the <code>ImageDescriptor</code>s.</p> */
  private static Map<String, ImageDescriptor> imageDescriptors;

  private static URL baseUrl;

  static {
    String pathSuffix = "icons/full/";
    try {
      Bundle bundle = HaskellUIPlugin.getDefault().getBundle();
      baseUrl = new URL( bundle.getEntry( "/" ), pathSuffix );
    } catch( MalformedURLException e ) {
      // do nothing
    }
  }

  private final static String OBJECT = "obj16/";   // basic colors - size 16x16
  private final static String OVR    = "ovr16/";   // basic colors - size 7x8
  private final static String ACTION = "clcl16/";  // basic colors - size 16x16
  private final static String WIZARD = "wizban/";  // wizard banners


  private static void declareImages() {
    // objects
    declare( LAUNCH_TAB_ARGUMENTS, OBJECT + "arguments_tab.gif" );
    declare( IMPORT_LIBRARY,       OBJECT + "library.gif" );
    declare( SOURCE_FOLDER,        OBJECT + "srcfolder.gif" );
    declare( SOURCE_FILE,          OBJECT + "srcfile.gif" );
    declare( LITERATE_SOURCE_FILE, OBJECT + "litsrcfile.gif" );
    declare( PROJECT_EXECUTABLE,   OBJECT + "projexe.gif" );
    declare( HASKELL_PROJECT,      OBJECT + "hsproject.gif" );
    // decorators
    declare( SRC_FOLDER_DECORATOR, OVR + "sourceFolder.gif" );
    declare( BIN_FOLDER_DECORATOR, OVR + "binaryFolder.gif" );
    // views
    declare( DEP_VIEW_IMPORTS,     ACTION + "mdep_imports.gif" );
    declare( DEP_VIEW_IMPORTEDBY,  ACTION + "mdep_importedby.gif" );
    declare( MB_VIEW_FLAT,         ACTION + "mb_flat.gif" );
    declare( MB_VIEW_HIERARCHICAL, ACTION + "mb_hierarchical.gif" );
    declare( MB_VIEW_FILTER,       ACTION + "mb_filter.gif" );
    declare( CO_VIEW_CLEAR,        ACTION + "co_clear.gif" );

    // language element representation
    declare( HS_NAME,               OBJECT + "hsname.gif" );
    declare( MODULE,                OBJECT + "module.gif" );
    declare( IMPORT,                OBJECT + "import.gif" );
    declare( IMPORT_GROUP,          OBJECT + "importgroup.gif" );
    declare( EXPORT_GROUP,          OBJECT + "exportgroup.gif" );
    declare( EXPORT_MODULE_CONTENT, OBJECT + "exportmodulecontent.gif" );
    declare( EXPORT_SPECIFICATION,  OBJECT + "exportspecification.gif" );
    declare( PACKAGE,               OBJECT + "package.gif" );
    declare( PACKAGE_CONF,          OBJECT + "packageconf.gif" );
    declare( PACKAGE_FOLDER,        OBJECT + "packagefolder.gif" );
    declare( HIDDEN_PACKAGE,        OBJECT + "hiddenpackage.gif" );
    declare( FUNCTION_BINDING,      OBJECT + "functionbinding.gif" );
    declare( PATTERN_BINDING,       OBJECT + "patternbinding.gif" );
    declare( DATA_DECL,             OBJECT + "datadecl.gif" );
    declare( TYPE_DECL,             OBJECT + "typedecl.gif" );
    declare( NEWTYPE_DECL,          OBJECT + "typedecl.gif" );
    declare( TYPE_SIGNATURE,        OBJECT + "typesig.gif" );
    declare( DEFAULT_DECL,          OBJECT + "defaultdecl.gif" );
    declare( CLASS_DECL,            OBJECT + "classdecl.gif" );
    declare( INSTANCE_DECL,         OBJECT + "instancedecl.gif" );
    declare( INFIXNONE_DECL,        OBJECT + "infixnone.gif" );
    declare( INFIXL_DECL,           OBJECT + "infixl.gif" );
    declare( INFIXR_DECL,           OBJECT + "infixr.gif" );

    // wizard banners
    declare( NEW_PROJECT, WIZARD + "newproject.gif" );
    declare( NEW_MODULE, WIZARD  + "newmodule.png" );
  }

  private final static void declare( final String key,
                                                  final String path ) {
    ImageDescriptor desc = ImageDescriptor.getMissingImageDescriptor();
    try {
      desc = ImageDescriptor.createFromURL( makeIconFileURL( path ) );
    } catch( MalformedURLException mux ) {
      HaskellUIPlugin.log( "Problem loading image.", mux );
    }
    imageRegistry.put( key, desc );
    imageDescriptors.put( key, desc );
  }

  private static ImageRegistry getImageRegistry() {
    if( imageRegistry == null ) {
      initializeImageRegistry();
    }
    return imageRegistry;
  }

  public static ImageRegistry initializeImageRegistry() {
    imageRegistry = new ImageRegistry( HaskellUIPlugin.getStandardDisplay() );
    imageDescriptors = new HashMap<String, ImageDescriptor>( 30 );
    declareImages();
    return imageRegistry;
  }

  /** <p>returns the <code>Image<code> identified by the given key,
    * or <code>null</code> if it does not exist.</p> */
  public static Image getImage( final String key ) {
    return getImageRegistry().get( key );
  }

  /** <p>returns the <code>ImageDescriptor<code> identified by the given key,
    * or <code>null</code> if it does not exist.</p> */
  public static ImageDescriptor getImageDescriptor( final String key ) {
    if( imageDescriptors == null ) {
      initializeImageRegistry();
    }
    return imageDescriptors.get( key );
  }

  private static URL makeIconFileURL( final String iconPath )
                                                  throws MalformedURLException {
    if( baseUrl == null ) {
      throw new MalformedURLException();
    }
    return new URL( baseUrl, iconPath );
  }


  // composite images
  ///////////////////

  public static Image getImage( final CompositeImageDescriptor imgDescriptor ) {
    if( imageDescriptorRegistry == null ) {
      imageDescriptorRegistry = new ImageDescriptorRegistry();
    }
    return imageDescriptorRegistry.get( imgDescriptor );
  }

  public static void disposeImageDescriptorRegistry() {
    if( imageDescriptorRegistry != null ) {
      imageDescriptorRegistry.dispose();
    }
  }
}