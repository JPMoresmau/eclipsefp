// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ghccompiler.ui.internal.util;

import org.eclipse.osgi.util.NLS;

/** <p>provides access to the internationalized UI texts.</p>
  *
  * @author Leif Frenzel
  */
public final class UITexts extends NLS {

  // message fields
  public static String generalTab_extra;
  public static String generalTab_extraMsg;

  public static String ghciTab_note;
  public static String ghciTab_options;
  public static String ghciTab_srcFolders;

  public static String ghcPreferencePage_desc;
  public static String ghcPreferencePage_general;
  public static String ghcPreferencePage_language;
  public static String ghcPreferencePage_moreOptimization;
  public static String ghcPreferencePage_optimization;

  public static String levelSelectionDialogField_levelInfo;
  public static String levelSelectionDialogField_zeroText;
  public static String levelSelectionDialogField_zeroTooltip;

  public static String optimizationTab_general;
  public static String optimizationTab_individual;
  public static String optimizationTab_individualInfo;

  public static String error_launchGhc;
  public static String error_processOutput;
  public static String error_processError;
  public static String error_deleteMarkers;
  public static String error_applyMarkers;

  private static final String BUNDLE_NAME
    = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}