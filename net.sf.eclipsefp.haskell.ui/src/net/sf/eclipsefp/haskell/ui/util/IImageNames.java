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

  String LAUNCH_TAB_ARGUMENTS = ID + ".LAUNCH_TAB_ARGUMENTS"; //$NON-NLS-1$
  String LAUNCH_TAB_AUTOMATION = ID + ".LAUNCH_TAB_AUTOMATION"; //$NON-NLS-1$

  String SRC_FOLDER_DECORATOR = ID + ".SRC_FOLDER_DECORATOR"; //$NON-NLS-1$

  String DEP_VIEW_IMPORTS     = ID + ".DEP_VIEW_IMPORTS"; //$NON-NLS-1$
  String DEP_VIEW_IMPORTEDBY  = ID + ".DEP_VIEW_IMPORTEDBY"; //$NON-NLS-1$

  String MB_VIEW_FLAT         = ID + ".MB_VIEW_FLAT"; //$NON-NLS-1$
  String MB_VIEW_HIERARCHICAL = ID + ".MB_VIEW_HIERARCHICAL"; //$NON-NLS-1$
  String MB_VIEW_FILTER       = ID + ".MB_VIEW_FILTER"; //$NON-NLS-1$
  String CO_VIEW_CLEAR        = ID + ".CO_VIEW_CLEAR"; //$NON-NLS-1$

  // Haskell project elements
  String HASKELL_PROJECT      = ID + ".HASKELL_PROJECT"; //$NON-NLS-1$
  String SOURCE_FOLDER        = ID + ".SOURCE_FOLDER"; //$NON-NLS-1$
  String FOLDER               = ID + ".FOLDER"; //$NON-NLS-1$
  String SOURCE_FILE          = ID + ".SOURCE_FILE"; //$NON-NLS-1$
  String LITERATE_SOURCE_FILE = ID + ".LITERATE_SOURCE_FILE"; //$NON-NLS-1$
  String IMPORT_LIBRARY       = ID + ".IMPORT_LIBRARY"; //$NON-NLS-1$
  String PROJECT_EXECUTABLE   = ID + ".PROJECT_EXECUTABLE"; //$NON-NLS-1$

  // Haskell language elements
  String HS_NAME               = ID + ".HS_NAME"; //$NON-NLS-1$
  String MODULE                = ID + ".MODULE"; //$NON-NLS-1$
  String IMPORT                = ID + ".IMPORT"; //$NON-NLS-1$
  String IMPORT_GROUP          = ID + ".IMPORT_GROUP"; //$NON-NLS-1$
  String EXPORT_GROUP          = ID + ".EXPORT_GROUP"; //$NON-NLS-1$
  String EXPORT_MODULE_CONTENT = ID + ".EXPORT_MODULE_CONTENT"; //$NON-NLS-1$
  String EXPORT_SPECIFICATION  = ID + ".EXPORT_SPECIFICATION"; //$NON-NLS-1$
  String PACKAGE               = ID + ".PACKAGE"; //$NON-NLS-1$
  String PACKAGE_CONF          = ID + ".PACKAGE_CONF"; //$NON-NLS-1$
  String PACKAGE_FOLDER        = ID + ".PACKAGE_FOLDER"; //$NON-NLS-1$
  String HIDDEN_PACKAGE        = ID + ".HIDDEN_PACKAGE"; //$NON-NLS-1$
  String FUNCTION_BINDING      = ID + ".FUNCTION_BINDING"; //$NON-NLS-1$
  String PATTERN_BINDING       = ID + ".PATTERN_BINDING"; //$NON-NLS-1$
  String DATA_DECL             = ID + ".DATA_DECL"; //$NON-NLS-1$
  String CONSTRUCTOR_DECL      = ID + ".CONSTRUCTOR_DECL"; //$NON-NLS-1$
  String SPLICE_DECL          = ID + ".SPLICE_DECL"; //$NON-NLS-1$
  String TYPE_SIGNATURE        = ID + ".TYPE_SIGNATURE"; //$NON-NLS-1$
  String TYPE_DECL             = ID + ".TYPE_DECL"; //$NON-NLS-1$
  String NEWTYPE_DECL          = ID + ".NEWTYPE_DECL"; //$NON-NLS-1$
  String CLASS_DECL            = ID + ".CLASS_DECL"; //$NON-NLS-1$
  String INSTANCE_DECL         = ID + ".INSTANCE_DECL"; //$NON-NLS-1$
  String DEFAULT_DECL          = ID + ".DEFAULT_DECL"; //$NON-NLS-1$
  String INFIXNONE_DECL        = ID + ".INFIXNONE_DECL"; //$NON-NLS-1$
  String INFIXL_DECL           = ID + ".INFIXL_DECL"; //$NON-NLS-1$
  String INFIXR_DECL           = ID + ".INFIXR_DECL"; //$NON-NLS-1$
  String FIELD_DECL           = ID + ".FIELD_DECL"; //$NON-NLS-1$

  // Cabal elements
  String EXECUTABLE_STANZA = ID + ".EXECUTABLE_STANZA"; //$NON-NLS-1$
  String TESTSUITE_STANZA  = ID + ".TESTSUITE_STANZA"; //$NON-NLS-1$
  String LIBRARY_STANZA    = ID + ".LIBRARY_STANZA"; //$NON-NLS-1$
  String GENERAL_STANZA    = ID + ".GENERAL_STANZA"; //$NON-NLS-1$
  String IF_STANZA         = ID + ".IF_STANZA"; //$NON-NLS-1$
  String ELSE_STANZA       = ID + ".ELSE_STANZA"; //$NON-NLS-1$
  String FLAG_STANZA       = ID + ".FLAG_STANZA"; //$NON-NLS-1$
  String TEMPLATE          = ID + ".TEMPLATE"; //$NON-NLS-1$
  String SOURCEREP_STANZA  = ID + "SOURCEREP_STANZA"; //$NON-NLS-1$

  // wizard banners
  String NEW_PROJECT          = ID + ".NEW_PROJECT"; //$NON-NLS-1$
  String NEW_MODULE           = ID + ".NEW_MODULE"; //$NON-NLS-1$
  String IMPORT_CABAL_PACKAGE = ID + "IMPORT_CABAL_PACKAGE"; //$NON-NLS-1$

  // overlays
  String ERROR_OVERLAY        = ID + ".ERROR_OVERLAY"; //$NON-NLS-1$
  String WARNING_OVERLAY      = ID + ".WARNING_OVERLAY"; //$NON-NLS-1$
  String ERROR_OBJECT        = ID + ".ERROR_OBJECT"; //$NON-NLS-1$
  String FAILURE_OVERLAY        = ID + ".FAILURE_OVERLAY"; //$NON-NLS-1$
  String ACTION_SORT          = ID + ".ACTION_SORT"; //$NON-NLS-1$

  String IMPORT_REMOVE        = ID+".IMPORT_REMOVE"; //$NON-NLS-1$

  String HASKELL_MISC         = ID+".HASKELL_MISC"; //$NON-NLS-1$

  String EXPORT_SRC           = ID+".EXPORT_SRC"; //$NON-NLS-1$

  String HACKAGE_UPDATE= ID+".HACKAGE_UPDATE"; //$NON-NLS-1$
  String HACKAGE_INSTALL= ID+".HACKAGE_INSTALL"; //$NON-NLS-1$
  String AUTODETECT= ID+".AUTODETECT"; //$NON-NLS-1$

  String SEARCH_LINE=ID+".SEARCH_LINE"; //$NON-NLS-1$
  String CORRECTION          = ID + ".CORRECTION"; //$NON-NLS-1$

  String TERMINATE = ID+".TERMINATE"; //$NON-NLS-1$
  String TEST = ID+".TEST"; //$NON-NLS-1$
  String TEST_ERR = ID+".TEST_ERR"; //$NON-NLS-1$
  String TEST_FAIL = ID+".TEST_FAIL"; //$NON-NLS-1$
  String TEST_OK = ID+".TEST_OK"; //$NON-NLS-1$
  String TEST_RUN = ID+".TEST_RUN"; //$NON-NLS-1$
  String HISTORY_LIST = ID+".HISTORY_LIST"; //$NON-NLS-1$
  String REMOVE_ALL= ID+".REMOVE_ALL"; //$NON-NLS-1$
}