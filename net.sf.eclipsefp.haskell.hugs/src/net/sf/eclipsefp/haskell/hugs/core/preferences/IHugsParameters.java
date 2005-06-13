// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.core.preferences;

/** <p>contains constant defintions for command line paramters of HUGS.</p>
  * 
  * @author Leif Frenzel
  */
public interface IHugsParameters {

  // TODO: use the comprehensive document
  
  // Options
  String PRINT_STATISTICS        = "s";
  String PRINT_TYPE_AFTER_EVAL   = "t";
  String TERMINATE_ON_ERROR      = "f";
  String GC_NOTIFICATION         = "g";
  // TODO
  // Literate modules +l,-l,+e,-e
  String DISPLAY_DOTS_LOADING    = ".";
  String DISPLAY_NOTHING_LOADING = "q";
  String LIST_FILES_LOADED       = "w";
  String DETAILED_KIND_ERRORS    = "k";
  String USE_SHOW                = "u";
  String IMPORT_CHASING          = "i";
    
  // Advanced
  String ADV_PROMPT        = "-p";
  String ADV_REPEAT_STRING = "-r";

  String ADV_HEAP_SIZE               = "-h";
  String ADV_PREPROCESSOR_CMD        = "-F";
  String ADV_CONSTRAINT_CUTOFF_LIMIT = "-c";  
  
}
