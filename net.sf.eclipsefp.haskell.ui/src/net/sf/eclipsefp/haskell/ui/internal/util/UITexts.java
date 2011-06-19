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
  public static String hsImplementationDialog_name_default;
  public static String hsImplementationDialog_name_index;

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

  public static String cabalImplsBlock_installed;
  public static String cabalImplsBlock_colName;
  public static String cabalImplsBlock_colCabalInstallVersion;
  public static String cabalImplsBlock_colCabalLibraryVersion;
  public static String cabalImplsBlock_colCabalPath;
  public static String cabalImplsBlock_btnAdd;
  public static String cabalImplsBlock_btnEdit;
  public static String cabalImplsBlock_btnRemove;
  public static String cabalImplsBlock_btnAutoDetect;
  public static String cabalImplsBlock_dlgAdd;
  public static String cabalImplsBlock_dlgEdit;
  public static String cabalImplsBlock_noCabalInstallationSelected;
  public static String cabalImplsBlock_noCabalInstallations;
  public static String cabalImplsBlock_needScionExecutablePath;

  public static String cabalImplsDialog_name;
  public static String cabalImplsDialog_executablePath;
  public static String cabalImplsDialog_libVersion;
  public static String cabalImplsDialog_installVersion;
  public static String cabalImplsDialog_btnBrowse;
  public static String cabalImplsDialog_dlgBrowse;
  public static String cabalImplsDialog_invalidUserIdentifier;
  public static String cabalImplDialog_invalidCabalExecutable;
  public static String cabalImplsBlock_multipleImplsSelected;

  public static String scionServer_preferences_label;
  public static String scionServerBuiltIn_label;
  public static String scionServerExecutable_label;

  public static String scionBrowser_preferences_label;
  public static String scionBrowserExecutable_label;

  public static String autodetectButton_label;
  public static String autodetectButton_text;
  public static String autodetectButton_errorTitle;
  public static String autodetectButton_errorMessage;

  public static String forceRebuildButton_label;
  public static String forceRebuildButton_text;
  public static String cabalUpdateButton_label;
  public static String cabalUpdateButton_text;

  public static String executableFileFieldEditor_errorDoesNotExist;
  public static String executableFileFieldEditor_errorNotAbsolute;
  public static String executableFileFieldEditor_errorNotExecutable;

  public static String scionServerStartupError_title;
  public static String scionServerStartupError_message;

  public static String scionServerBuildJob;
  public static String scionServerChangeJob;
  public static String scionServerExecutableNotPresent;
  public static String scionServerAbnormalTermination_title;
  public static String scionServerAbnormalTermination_message;
  public static String scionRebuild_title;
  public static String scionRebuild_message;
  public static String scionRebuild_DirectoryExists_title;
  public static String scionRebuild_DirectoryExists_message;

  public static String scionServerFlavor_title;
  public static String scionServerFlavor_stdstream_label;
  public static String scionServerFlavor_network_label;

  public static String scionVerboseInteraction_title;

  public static String scionVersionMismatch_title;
  public static String scionVersionMismatch_message;

  public static String scionServerDoesntExist_title;
  public static String scionServerDoesntExist_message;

  public static String scionBrowserDoesntExist_title;
  public static String scionBrowserDoesntExist_message;

  public static String scionBrowserNotConfigured_title;
  public static String scionBrowserNotConfigured_message;

  public static String scionBrowserRebuildDatabase_title;
  public static String scionBrowserRebuildDatabase_message;

  public static String scionBrowserLoadingDatabases;

  public static String scionBrowserRebuildingDatabase;
  public static String scionBrowserRebuildingDatabaseError_title;
  public static String scionBrowserRebuildingDatabaseError_message;

  public static String scionServerInstallError;
  public static String scionServerInstallFailed;
  public static String cabalUpdateError;
  public static String cabalUpdateFailed;

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
  public static String newProjectWizardPage_Message_projectInvalidName;
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
  public static String newProjectWizardPage_Message_notExistingProjectOnWorkspaceRoot;
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
  public static String editor_occurrences_job;

  public static String scion_console_title;
  public static String scion_preferences_title;
  public static String scion_delta_error;

  public static String libraries_description;
  public static String libraries_add_title;
  public static String libraries_add_available_title;
  public static String libraries_add_component_title;
  public static String libraries_add_component_all;
  public static String libraries_add_component_selected;
  public static String libraries_add_version_title;
  public static String libraries_add_version_none;
  public static String libraries_add_version_specific;

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
  public static String module_inclusion_nosourcefolder;

  public static String quickfix_marker_annotation_name;
  public static String resolve_missingtype;
  public static String resolve_addpragma;
  public static String resolve_import_remove;
  public static String resolve_import_replace;
  public static String resolve_addpackage;

  public static String preferences_title;

  public static String preferences_project_title;
  public static String preferences_project_description;
  public static String preferences_project_folders;
  public static String preferences_project_use_project;
  public static String preferences_project_use_folders;
  public static String preferences_project_source;
  public static String preferences_project_documentation;
  public static String preferences_project_folders_empty;
  public static String preferences_project_invalid;
  public static String preferences_project_invalid_source;
  public static String preferences_project_invalid_documentation;
  public static String preferences_project_invalid_project;
  public static String preferences_project_output_note;

  public static String preferences_editor_description;
  public static String preferences_editor_description_hover;
  public static String preferences_editor_color;

  public static String preferences_editor_annotations_title;
  public static String preferences_editor_annotations_presentation;
  public static String preferences_editor_annotations_showinruler;
  public static String preferences_editor_annotations_showintext;

  public static String preferences_editor_appearance_title;
  public static String preferences_editor_appearance_line_number_color;
  public static String preferences_editor_appearance_matching_brackets_color;
  public static String preferences_editor_appearance_current_line_color;
  public static String preferences_editor_appearance_print_margin_color;
  public static String preferences_editor_appearance_color_options;
  public static String preferences_editor_appearance_print_margin_column;
  public static String preferences_editor_appearance_overview_ruler;
  public static String preferences_editor_appearance_line_numbers;
  public static String preferences_editor_appearance_matching_brackets;
  public static String preferences_editor_appearance_current_line;
  public static String preferences_editor_appearance_print_margin;

  public static String preferences_editor_contentass_background;
  public static String preferences_editor_contentass_foreground;
  public static String preferences_editor_contentass_color_options;
  public static String preferences_editor_contentass_autoinsert;
  public static String preferences_editor_contentass_alpha;
  public static String preferences_editor_contentass_autoactivation;
  public static String preferences_editor_contentass_autoactivation_delay;
  public static String preferences_editor_contentass_autoactivation_triggers;

  public static String preferences_editor_syntax_title;
  public static String preferences_editor_syntax_comments;
  public static String preferences_editor_syntax_literatecomments;
  public static String preferences_editor_syntax_strings;
  public static String preferences_editor_syntax_characters;
  public static String preferences_editor_syntax_functions;
  public static String preferences_editor_syntax_numbers;
  public static String preferences_editor_syntax_vars;
  public static String preferences_editor_syntax_cons;
  public static String preferences_editor_syntax_keywords;
  public static String preferences_editor_syntax_symbols;
  public static String preferences_editor_syntax_cpp;
  public static String preferences_editor_syntax_th;
  public static String preferences_editor_syntax_others;
  public static String preferences_editor_syntax_bold;
  public static String preferences_editor_syntax_foreground;
  public static String preferences_editor_syntax_background;
  public static String preferences_editor_syntax_systemdefault;
  public static String preferences_editor_syntax_custom;

  public static String preferences_editor_typing_title;
  public static String preferences_editor_typing_spaces_tabs;
  public static String preferences_editor_typing_tab_width;
  public static String preferences_editor_typing_cabal_tab_width;

  public static String preferences_debug_title;
  public static String preferences_debug_description;
  public static String preferences_debug_break_on_error;
  public static String preferences_debug_break_on_exception;
  public static String preferences_debug_print_with_show;

  public static String preferences_search_haddock_description;
  public static String preferences_search_haddock_help;
  public static String preferences_search_haddock_new;
  public static String preferences_search_haddock_new_empty;

  public static String explorer_libraries;
  public static String explorer_libraries_noimpl;
  public static String explorer_outline_open;

  public static String log_nodetails;
  public static String noproject;
  public static String sharedScionInstance_console;
  public static String sharedBrowserInstance_console;

  public static String unpackScionArchive_title;
  public static String scionBuildJob_title;
  public static String cabalUpdateJob_title;

  public static String scionArchiveResourceNotFound;
  public static String scionArchiveFileException;
  public static String scionArchiveNonspecificFileException;
  public static String scionServerProgress_title;
  public static String scionServerProgress_subtask1;
  public static String scionServerProgress_subtask2;
  public static String cabalUpdateProgress;
  // public static String scionServerProgress_completed_title;
  // public static String scionServerProgress_completed_message;

  public static String noCabalImplementation_title;
  public static String noCabalImplementation_message;
  public static String zerolenCabalExecutable_message;

  public static String openDefinition_open_job;
  public static String openDefinition_select_job;
  public static String openDefinition_external_job;

  public static String exportSource_job;
  public static String exportSource_error;
  public static String exportSource_warning;
  public static String exportSource_error_text;
  public static String exportSource_warning_text;
  public static String exportSource_options;
  public static String exportSource_options_folder;
  public static String exportSource_options_snapshot;
  public static String exportSource_options_folder_choose;
  public static String exportSource_warning_markers;
  public static String exportSource_overwrite_warning;

  public static String install_job;
  public static String install_options;
  public static String install_options_folder;
  public static String install_options_folder_choose;
  public static String install_options_user;
  public static String install_options_global;
  public static String install_error;
  public static String install_error_text;

  public static String test_job;
  public static String test_options;
  public static String test_options_folder;
  public static String test_options_folder_choose;
  public static String test_options_testsuites;
  public static String test_options_testsuites_all;
  public static String test_options_testsuites_none;
  public static String test_error;
  public static String test_error_text;
  public static String test_version_fail;

  public static String template_prefSave_backingStore_exception;
  public static String HaskellTemplateVariables_module_description;
  public static String HaskellTemplateVariables_classTypeName_description;
  public static String HaskellTemplateVariables_typeName_description;

  public static String HaddockDocumentation_not_applicable;
  public static String HaddockDocumentation_previous_item;
  public static String HaddockDocumentation_following_item;
  public static String HaddockDocumentation_user_replaces;

  public static String properties_userflags_description;
  public static String properties_userflags_none;

  public static String error_tabs;
  public static String error_tabs_message;

  public static String browser_definedInModules;
  public static String browser_packagedInPackages;

  private static final String BUNDLE_NAME = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}