// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.util;

import org.eclipse.osgi.util.NLS;

/** <p>provides access to the internationalized UI texts.</p>
  *
  * @author Leif Frenzel
  */
public final class UITexts extends NLS {

  // message fields
  public static String cabalFormEditor_tabSource;

  public static String cabalHyperLinkDetector_errorTitle;

  public static String cabalPackageImportWizard_pageDesc;
  public static String cabalPackageImportWizard_pageTitle;

  public static String descriptionSection_entryDescription;
  public static String descriptionSection_entryCategory;
  public static String descriptionSection_entryHomepage;
  public static String descriptionSection_entrySynopsis;
  public static String descriptionSection_title;

  public static String generalSection_entryAuthor;
  public static String generalSection_entryMaintainer;
  public static String generalSection_entryName;
  public static String generalSection_entryVersion;
  public static String generalSection_title;

  public static String hsImplementationDialog_binDir;
  public static String hsImplementationDialog_btnBrowse;
  public static String hsImplementationDialog_dlgBrowse;
  public static String hsImplementationDialog_duplicate;
  public static String hsImplementationDialog_libDir;
  public static String hsImplementationDialog_name;
  public static String hsImplementationDialog_type;
  public static String hsImplementationDialog_version;

  public static String implementationsBlock_btnAdd;
  public static String implementationsBlock_btnEdit;
  public static String implementationsBlock_btnRemove;
  public static String implementationsBlock_colName;
  public static String implementationsBlock_colType;
  public static String implementationsBlock_colVersion;
  public static String implementationsBlock_dlgAdd;
  public static String implementationsBlock_dlgEdit;
  public static String implementationsBlock_installed;

  public static String installedImplementationsPP_msg;
  public static String installedImplementationsPP_nothingSelected;

  public static String legalSection_entryCopyright;
  public static String legalSection_entryLicense;
  public static String legalSection_entryLicenseFile;
  public static String legalSection_title;

  public static String mkPointFree_refuseDlg_title;
  public static String mkPointFree_refuseDlg_message;

  public static String mkPointFreeProcessor_elem;
  public static String mkPointFreeProcessor_name;

  public static String mkPointFreeDelegate_checking;
  public static String mkPointFreeDelegate_collectingChanges;
  public static String mkPointFreeDelegate_noSelection;
  public static String mkPointFreeDelegate_noSourceFile;
  public static String mkPointFreeDelegate_notApplicable;
  public static String mkPointFreeDelegate_roFile;

  public static String newHaskellProjectWizard_pageDesc;
  public static String newHaskellProjectWizard_pageTitle;

  public static String overviewPage_title;


  private static final String BUNDLE_NAME
    = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}