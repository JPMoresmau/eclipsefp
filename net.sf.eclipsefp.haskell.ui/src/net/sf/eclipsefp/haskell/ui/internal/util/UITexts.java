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
  public static String cabalPackageImportWizard_windowTitle;

  public static String cabalPackageImportWP_PackageGroup_locationLabel_desc;
  public static String cabalPackageImportWP_LocationGroup_browseButton_desc;

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

  public static String scionServerExecutable_label;
  public static String autodetectButton_label;
  public static String autodetectButton_text;
  public static String autodetectButton_errorTitle;
  public static String autodetectButton_errorMessage;

  public static String executableFileFieldEditor_errorDoesNotExist;
  public static String executableFileFieldEditor_errorNotAbsolute;
  public static String executableFileFieldEditor_errorNotExecutable;

  public static String scionServerStartupError_title;
  public static String scionServerStartupError_message;

  public static String legalSection_entryCopyright;
  public static String legalSection_entryLicense;
  public static String legalSection_entryLicenseFile;
  public static String legalSection_title;

  public static String mkPointFree_refuseDlg_title;
  public static String mkPointFree_refuseDlg_message;

  public static String mkPointFreeProcessor_name;

  public static String mkPointFreeDelegate_collectingChanges;
  public static String mkPointFreeDelegate_notApplicable;

  public static String newHaskellProjectWizard_pageDesc;
  public static String newHaskellProjectWizard_pageTitle;
  public static String newHaskellProjectWizard_windowTitle;

  public static String newProjectWizardPage_NameGroup_label_text;
  public static String newProjectWizardPage_Message_enterProjectName;
  public static String newProjectWizardPage_Message_projectAlreadyExists;
  public static String newProjectWizardPageMessage_invalidProjectNameForWorkspaceRoot;
  public static String newProjectWizardPage_LocationGroup_title;
  public static String newProjectWizardPage_LocationGroup_workspace_desc;
  public static String newProjectWizardPage_LocationGroup_external_desc;
  public static String newProjectWizardPage_LocationGroup_locationLabel_desc;
  public static String newProjectWizardPage_LocationGroup_browseButton_desc;
  public static String newProjectWizardPage_directory_message;
  public static String newProjectWizardPage_Message_enterLocation;
  public static String newProjectWizardPage_Message_invalidDirectory;
  public static String newProjectWizardPage_Message_notOnWorkspaceRoot;
  public static String newProjectWizardPage_Message_notExisingProjectOnWorkspaceRoot;
  public static String newProjectWizardPage_Message_cannotCreateAtExternalLocation;

  public static String newProjectWizardPage_ComponentGroup_title;
  public static String newProjectWizardPage_ComponentGroup_executable;
  public static String newProjectWizardPage_ComponentGroup_library;

  public static String overviewPage_title;

  public static String refDelegate_checking;
  public static String refDelegate_noSelection;
  public static String refDelegate_noSourceFile;
  public static String refDelegate_roFile;

  public static String refProcessor_elem;

  public static String renameProcessor_name;

  public static String editor_textHover_error;
  public static String editor_actions_source;

  public static String scion_console_title;
  public static String scion_preferences_title;
  public static String scion_delta_error;

  public static String outline_sortByName;
  public static String outline_sortByName_tooltip;
  public static String outline_sortByName_description;

  public static String module_inclusion_title;
  public static String module_inclusion_field_section;
  public static String module_inclusion_field_include;
  public static String module_inclusion_field_expose;
  public static String module_inclusion_page_title;
  public static String module_inclusion_page_description;
  public static String module_inclusion_error;

  public static String quickfix_marker_annotation_name;
  public static String resolve_missingtype;
  public static String resolve_addpragma;

  public static String preferences_project_title;
  public static String preferences_project_description;
  public static String preferences_project_folders;
  public static String preferences_project_use_project;
  public static String preferences_project_use_folders;
  public static String preferences_project_source;
  public static String preferences_project_folders_empty;
  public static String preferences_project_invalid;
  public static String preferences_project_invalid_source;
  public static String preferences_project_invalid_project;
  public static String preferences_project_output_note;


  private static final String BUNDLE_NAME
    = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}