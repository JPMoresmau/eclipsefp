// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.preferences;


/** <p>contains constants for preference names in the Haskell UI.</p>
  * 
  * @author Leif Frenzel
  */
public interface IPreferenceConstants {

  // Haskell compiler
  String SHOW_COMPILER_LOG = "SHOW_COMPILER_LOG";
  
  // new Haskell project
  String TARGET_BINARY = "TARGET_BINARY";
  String FOLDERS_BIN = "FOLDERS_BIN";
  String FOLDERS_SRC = "FOLDERS_SRC";
  String FOLDERS_OUT = "FOLDERS_OUT";
  String FOLDERS_IN_NEW_PROJECT = "FOLDERS_IN_NEW_PROJECT";
 
  // code assist
  String AUTO_ACTIVATION_DELAY  = "AUTO_ACTIVATION_DELAY";
  String AUTO_INSERT            = "AUTO_INSERT";
  String ENABLE_AUTO_ACTIVATION = "ENABLE_AUTO_ACTIVATION";
  String INSERT_COMPLETION      = "INSERT_COMPLETION";
  String ORDER_PROPOSALS        = "ORDER_PROPOSALS";
  String SHOW_VISIBLE_PROPOSALS = "SHOW_VISIBLE_PROPOSALS";
}