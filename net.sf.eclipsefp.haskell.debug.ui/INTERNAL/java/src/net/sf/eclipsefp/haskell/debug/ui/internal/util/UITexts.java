// Copyright (c) 2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.util;

import org.eclipse.osgi.util.NLS;

/** <p>provides access to the internationalized UI texts.</p>
  *
  * @author Leif Frenzel
  */
public final class UITexts extends NLS {

  // message fields
  public static String executableLaunchOperations_errorMsg;

  public static String haskellArgumentsTab_background;
  public static String haskellArgumentsTab_browseFs;
  public static String haskellArgumentsTab_browseWs;
  public static String haskellArgumentsTab_lblArguments;
  public static String haskellArgumentsTab_name;
  public static String haskellArgumentsTab_noDir;
  public static String haskellArgumentsTab_noteQuote;
  public static String haskellArgumentsTab_noWorkDir;
  public static String haskellArgumentsTab_selectDir;
  public static String haskellArgumentsTab_workDir;

  public static String launchOperation_msg;
  public static String launchOperation_title;





  private static final String BUNDLE_NAME
    = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}