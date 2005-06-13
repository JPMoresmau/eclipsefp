// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.core;

/** <p>contains constants for the Haddock command line options.</p>
  * 
  * @author Leif Frenzel
  */
public interface IHaddockParameters {

  // generation
  String NO_PRELUDE   = "-n";
  String PACKAGE_NAME = "-k";
  String TITLE        = "-t";
  String PROLOGUE     = "-p";
  String CSS_FILE     = "-c";


  // directory settings
  String OH_DEAR                = "-o";
  String READ_HADDOCK_INTERFACE = "-i";
  String DUMP_HADDOCK_INTERFACE = "-D";
  
  // misc
  String VERSION = "-V";

  // format
  
  // output in HTML
  String FORMAT_HTML = "-h";
  // "--html-help=format" produce index and table of contents 
  //                      in mshelp, mshelp2 or devhelp format (with -h)
  String MS_HTML_HELP = "--html-help";
  
  // "--use-contents=URL" use a separately-generated HTML contents page
  String USE_CONTENTS      = "--use-contents";
  // "--use-index=URL" use a separately-generated HTML index
  String USE_INDEX         = "--use-index";
  
  // TODO to support these scenarios doesn't fit so easy in the wizard
  
  // "--gen-contents" generate an HTML contents from specified interfaces
  String GENERATE_CONTENTS = "--gen-contents";
  // "--gen-index" generate an HTML index from specified interfaces
  String GENERATE_INDEX    = "--gen-index";

  // "--lib=DIR" location of Haddock's auxiliary files
//  String LIB_DIR = "--lib";
//  String SOURCE_URL   = "-s";  
}
