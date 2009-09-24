// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

/** <p>contains constant names for images in the Haskell UI.</p>
  *
  * @author Leif Frenzel
  */
public interface IImageNames {

  // prefix all constants with the plugin id
  String ID = HaskellUIPlugin.getPluginId();

  String LAUNCH_TAB_ARGUMENTS = ID + ".LAUNCH_TAB_ARGUMENTS";

  String SRC_FOLDER_DECORATOR = ID + ".SRC_FOLDER_DECORATOR";

  String DEP_VIEW_IMPORTS     = ID + ".DEP_VIEW_IMPORTS";
  String DEP_VIEW_IMPORTEDBY  = ID + ".DEP_VIEW_IMPORTEDBY";

  String MB_VIEW_FLAT         = ID + ".MB_VIEW_FLAT";
  String MB_VIEW_HIERARCHICAL = ID + ".MB_VIEW_HIERARCHICAL";
  String MB_VIEW_FILTER       = ID + ".MB_VIEW_FILTER";
  String CO_VIEW_CLEAR        = ID + ".CO_VIEW_CLEAR";

  // Haskell project elements
  String HASKELL_PROJECT      = ID + ".HASKELL_PROJECT";
  String SOURCE_FOLDER        = ID + ".SOURCE_FOLDER";
  String SOURCE_FILE          = ID + ".SOURCE_FILE";
  String LITERATE_SOURCE_FILE = ID + ".LITERATE_SOURCE_FILE";
  String IMPORT_LIBRARY       = ID + ".IMPORT_LIBRARY";
  String PROJECT_EXECUTABLE   = ID + ".PROJECT_EXECUTABLE";

  // Haskell language elements
  String HS_NAME               = ID + ".HS_NAME";
  String MODULE                = ID + ".MODULE";
  String IMPORT                = ID + ".IMPORT";
  String IMPORT_GROUP          = ID + ".IMPORT_GROUP";
  String EXPORT_GROUP          = ID + ".EXPORT_GROUP";
  String EXPORT_MODULE_CONTENT = ID + ".EXPORT_MODULE_CONTENT";
  String EXPORT_SPECIFICATION  = ID + ".EXPORT_SPECIFICATION";
  String PACKAGE               = ID + ".PACKAGE";
  String PACKAGE_CONF          = ID + ".PACKAGE_CONF";
  String PACKAGE_FOLDER        = ID + ".PACKAGE_FOLDER";
  String HIDDEN_PACKAGE        = ID + ".HIDDEN_PACKAGE";
  String FUNCTION_BINDING      = ID + ".FUNCTION_BINDING";
  String PATTERN_BINDING       = ID + ".PATTERN_BINDING";
  String DATA_DECL             = ID + ".DATA_DECL";
  String TYPE_SIGNATURE        = ID + ".TYPE_SIGNATURE";
  String TYPE_DECL             = ID + ".TYPE_DECL";
  String NEWTYPE_DECL          = ID + ".NEWTYPE_DECL";
  String CLASS_DECL            = ID + ".CLASS_DECL";
  String INSTANCE_DECL         = ID + ".INSTANCE_DECL";
  String DEFAULT_DECL          = ID + ".DEFAULT_DECL";
  String INFIXNONE_DECL        = ID + ".INFIXNONE_DECL";
  String INFIXL_DECL           = ID + ".INFIXL_DECL";
  String INFIXR_DECL           = ID + ".INFIXR_DECL";
  String FIELD_DECL           = ID + ".FIELD_DECL";

  // Cabal elements
  String EXECUTABLE_STANZA = ID + ".EXECUTABLE_STANZA"; //$NON-NLS-1$
  String LIBRARY_STANZA    = ID + ".LIBRARY_STANZA"; //$NON-NLS-1$
  String GENERAL_STANZA    = ID + ".GENERAL_STANZA"; //$NON-NLS-1$
  String TEMPLATE          = ID + ".TEMPLATE"; //$NON-NLS-1$

  // wizard banners
  String NEW_PROJECT          = ID + ".NEW_PROJECT";
  String NEW_MODULE           = ID + ".NEW_MODULE";
  String IMPORT_CABAL_PACKAGE = ID + "IMPORT_CABAL_PACKAGE";

  // overlays
  String ERROR_OVERLAY        = ID + ".ERROR_OVERLAY";
  String WARNING_OVERLAY      = ID + ".WARNING_OVERLAY";

  String ACTION_SORT          = ID + ".ACTION_SORT";
}