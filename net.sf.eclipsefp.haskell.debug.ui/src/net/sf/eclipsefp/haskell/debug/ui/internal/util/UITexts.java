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
  public static String haskellArgumentsTab_lblFullArguments;
  public static String haskellArgumentsTab_name;
  public static String haskellArgumentsTab_noDir;
  public static String haskellArgumentsTab_noteQuote;
  public static String haskellArgumentsTab_noWorkDir;
  public static String haskellArgumentsTab_selectDir;
  public static String haskellArgumentsTab_workDir;
  public static String haskellArgumentsTab_syncStreams;
  public static String haskellArgumentsTab_lblStanza;
  public static String haskellArgumentsTab_noStanza;

  public static String haskellAutomationTab_name;
  public static String haskellAutomationTab_command;
  public static String haskellAutomationTab_reload;
  public static String haskellAutomationTab_command_reload;


  public static String launchOperation_msg;
  public static String launchOperation_title;

  public static String configuration_name;

  public static String error_read_configuration;

  public static String debug_editor_title;
  public static String debug_editor_text;

  public static String breakpoint_toggle;
  public static String breakpoint_toggle_description;

  public static String evaluate_need_suspend;

  public static String profiling_name;

  public static String runExecutable;
  public static String runProfiling;
  public static String runTestSuite;
  public static String runTestFramework;
  public static String yesod_devel_error;

  public static String command_history;

  public static String test_view_runs;
  public static String test_view_errors;
  public static String test_view_failures;
  public static String test_view_text;
  public static String test_history;
  public static String test_history_clear;
  public static String test_history_none;

  private static final String BUNDLE_NAME
    = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}