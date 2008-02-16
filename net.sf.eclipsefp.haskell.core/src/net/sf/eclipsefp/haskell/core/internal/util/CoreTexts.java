// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.util;

import org.eclipse.osgi.util.NLS;

/** <p>provides internationalized String messages for the core.</p>
  *
  * @author Leif Frenzel
  */
public final class CoreTexts extends NLS {

  // message fields
  public static String checkOutFoldersOperation_checkingBin;
  public static String checkOutFoldersOperation_checkingOutput;
  public static String checkOutFoldersOperation_cleaning;

  public static String cleanOutFoldersOperation_cleaning;
  public static String cleanOutFoldersOperation_removingExes;
  public static String cleanOutFoldersOperation_shrubbingOut;

  public static String compilerManager_noNamePlaceHolder;

  public static String haskellBuilder_full;
  public static String haskellBuilder_compiling;
  public static String haskellBuilder_refreshing;

  public static String haskellLaunchDelegate_noExe;

  public static String haskellProjectManager_jobName;

  public static String projectCreationOperation_creating;
  public static String projectCreationOperation_dirs;
  public static String projectCreationOperation_init;
  public static String projectCreationOperation_natures;
  public static String projectCreationOperation_settings;

  public static String sourceFileGenerator_creating;


  private static final String BUNDLE_NAME
    = CoreTexts.class.getPackage().getName() + ".coretexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, CoreTexts.class );
  }
}