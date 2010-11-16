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
    String pathSuffix = "icons/";  //$NON-NLS-1$
    try {
      Bundle bundle = HaskellUIPlugin.getDefault().getBundle();
      baseUrl = new URL( bundle.getEntry( "/" ), pathSuffix ); //$NON-NLS-1$
    } catch( MalformedURLException e ) {
      // do nothing
    }
  }

  private final static String OBJECT = "obj16/";   // basic colors - size 16x16 //$NON-NLS-1$
  private final static String OVR    = "ovr16/";   // basic colors - size 7x8 //$NON-NLS-1$
  private final static String ACTION = "clcl16/";  // basic colors - size 16x16 //$NON-NLS-1$
  private final static String WIZARD = "wizban/";  // wizard banners //$NON-NLS-1$
  private final static String ACTION_E = "elcl16/"; //$NON-NLS-1$
  private final static String TOOLS = "etool16/"; //$NON-NLS-1$

  private static void declareImages() {
    // objects
    declare( LAUNCH_TAB_ARGUMENTS, OBJECT + "arguments_tab.gif" ); //$NON-NLS-1$
    declare( LAUNCH_TAB_AUTOMATION, OBJECT + "automation_tab.gif" ); //$NON-NLS-1$
    declare( IMPORT_LIBRARY,       OBJECT + "library.gif" ); //$NON-NLS-1$
    declare( SOURCE_FOLDER,        OBJECT + "srcfolder_obj.gif" ); //$NON-NLS-1$
    declare( SOURCE_FILE,          OBJECT + "hsfile_obj.gif" ); //$NON-NLS-1$
    declare( LITERATE_SOURCE_FILE, OBJECT + "lhsfile_obj.gif" ); //$NON-NLS-1$
    declare( PROJECT_EXECUTABLE,   OBJECT + "projexe.gif" ); //$NON-NLS-1$
    declare( HASKELL_PROJECT,      OBJECT + "hsproject.gif" ); //$NON-NLS-1$
    declare( EXECUTABLE_STANZA,    OBJECT + "executablestanza.gif" ); //$NON-NLS-1$
    declare( LIBRARY_STANZA,       OBJECT + "librarystanza.gif" ); //$NON-NLS-1$
    declare( GENERAL_STANZA,       OBJECT + "generalstanza.gif" ); //$NON-NLS-1$
    declare( IF_STANZA,            OBJECT + "if.gif" ); //$NON-NLS-1$
    declare( ELSE_STANZA,          OBJECT + "else.gif" ); //$NON-NLS-1$
    declare( FLAG_STANZA,          OBJECT + "flag.gif" ); //$NON-NLS-1$
    declare( TEMPLATE,             OBJECT + "template.gif" ); //$NON-NLS-1$

    // decorators
    declare( SRC_FOLDER_DECORATOR, OVR + "sourceFolder.gif" ); //$NON-NLS-1$
    declare( ERROR_OVERLAY,        OVR + "error_co.gif"); //$NON-NLS-1$
    declare( WARNING_OVERLAY,      OVR + "warning_co.gif"); //$NON-NLS-1$

    // views
    declare( DEP_VIEW_IMPORTS,     ACTION + "mdep_imports.gif" ); //$NON-NLS-1$
    declare( DEP_VIEW_IMPORTEDBY,  ACTION + "mdep_importedby.gif" ); //$NON-NLS-1$
    declare( MB_VIEW_FLAT,         ACTION + "mb_flat.gif" ); //$NON-NLS-1$
    declare( MB_VIEW_HIERARCHICAL, ACTION + "mb_hierarchical.gif" ); //$NON-NLS-1$
    declare( MB_VIEW_FILTER,       ACTION + "mb_filter.gif" ); //$NON-NLS-1$
    declare( CO_VIEW_CLEAR,        ACTION + "co_clear.gif" ); //$NON-NLS-1$

    // language element representation
    declare( HS_NAME,               OBJECT + "hsname.gif" ); //$NON-NLS-1$
    declare( MODULE,                OBJECT + "module.gif" ); //$NON-NLS-1$
    declare( IMPORT,                OBJECT + "import.gif" ); //$NON-NLS-1$
    declare( IMPORT_GROUP,          OBJECT + "importgroup.gif" ); //$NON-NLS-1$
    declare( EXPORT_GROUP,          OBJECT + "exportgroup.gif" ); //$NON-NLS-1$
    declare( EXPORT_MODULE_CONTENT, OBJECT + "exportmodulecontent.gif" ); //$NON-NLS-1$
    declare( EXPORT_SPECIFICATION,  OBJECT + "exportspecification.gif" ); //$NON-NLS-1$
    declare( PACKAGE,               OBJECT + "package.gif" ); //$NON-NLS-1$
    declare( PACKAGE_CONF,          OBJECT + "packageconf.gif" ); //$NON-NLS-1$
    declare( PACKAGE_FOLDER,        OBJECT + "packagefolder.gif" ); //$NON-NLS-1$
    declare( HIDDEN_PACKAGE,        OBJECT + "hiddenpackage.gif" ); //$NON-NLS-1$
    declare( FUNCTION_BINDING,      OBJECT + "functionbinding.gif" ); //$NON-NLS-1$
    declare( PATTERN_BINDING,       OBJECT + "patternbinding.gif" ); //$NON-NLS-1$
    declare( DATA_DECL,             OBJECT + "datadecl.gif" ); //$NON-NLS-1$
    declare( TYPE_DECL,             OBJECT + "typedecl.gif" ); //$NON-NLS-1$
    declare( NEWTYPE_DECL,          OBJECT + "typedecl.gif" ); //$NON-NLS-1$
    declare( TYPE_SIGNATURE,        OBJECT + "typesig.gif" ); //$NON-NLS-1$
    declare( DEFAULT_DECL,          OBJECT + "defaultdecl.gif" ); //$NON-NLS-1$
    declare( CLASS_DECL,            OBJECT + "classdecl.gif" ); //$NON-NLS-1$
    declare( INSTANCE_DECL,         OBJECT + "instancedecl.gif" ); //$NON-NLS-1$
    declare( INFIXNONE_DECL,        OBJECT + "infixnone.gif" ); //$NON-NLS-1$
    declare( INFIXL_DECL,           OBJECT + "infixl.gif" ); //$NON-NLS-1$
    declare( INFIXR_DECL,           OBJECT + "infixr.gif" ); //$NON-NLS-1$
    declare( FIELD_DECL,            OBJECT + "field_public_obj.gif")  ; //$NON-NLS-1$
    // wizard banners
    declare( NEW_PROJECT, WIZARD + "newhprj_wiz.png" ); //$NON-NLS-1$
    declare( NEW_MODULE, WIZARD  + "newmodule.png" ); //$NON-NLS-1$
    declare( IMPORT_CABAL_PACKAGE, WIZARD + "impcprj_wiz.png" ); //$NON-NLS-1$

    declare(ACTION_SORT,ACTION_E+"alphab_sort_co.png"); //$NON-NLS-1$
    declare(IMPORT_REMOVE,OBJECT+"correction_delete_import.gif"); //$NON-NLS-1$
    declare(EXPORT_SRC,OBJECT+"export_src.gif"); //$NON-NLS-1$
    declare(HASKELL_MISC,TOOLS+"haskell_misc.gif"); //$NON-NLS-1$
  }

  private final static void declare( final String key,
                                                  final String path ) {
    ImageDescriptor desc = ImageDescriptor.getMissingImageDescriptor();
    try {
      desc = ImageDescriptor.createFromURL( makeIconFileURL( path ) );
    } catch( MalformedURLException mux ) {
      HaskellUIPlugin.log( "Problem loading image.", mux ); //$NON-NLS-1$
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