// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.util;

import org.eclipse.osgi.util.NLS;

/**
 * <p>
 * provides access to the internationalized UI texts.
 * </p>
 *
 * @author Leif Frenzel
 */
public final class UITexts extends NLS {

  // message fields
  public static String dots;

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
  public static String generalSection_entryStability;
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
  public static String cabalImplsBlock_needBrowserExecutablePath;
  public static String cabalImplsBlock_needBuildWrapperExecutablePath;

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
  public static String scionBrowserBuiltIn_label;
  public static String scionBrowserExecutable_label;
  public static String scionBrowserUseHackage_label;
  public static String scionBrowserUseHackage_QuestionNew_title;
  public static String scionBrowserUseHackage_QuestionNew_label;
  public static String scionBrowserUseHackage_QuestionUpdate_title;
  public static String scionBrowserUseHackage_QuestionUpdate_label;

  public static String autodetectButton_label;
  public static String autodetectButton_text;
  public static String autodetectButton_errorTitle;
  public static String autodetectButton_errorMessage;
  public static String installHackageButton_label;
  public static String installHackageButton_text;
  public static String installHackageButton_errorTitle;
  public static String executable_label;

  public static String ignore_missing_button;
  public static String ignore_tooold_button;

  public static String forceRebuildButton_label;
  public static String forceRebuildButton_text;
  public static String forceRebuildBrowserButton_text;
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

  public static String browserServerBuildJob;
  public static String browserServerChangeJob;
  public static String browserServerExecutableNotPresent;
  public static String browserServerAbnormalTermination_title;
  public static String browserServerAbnormalTermination_message;
  public static String browserRebuild_title;
  public static String browserRebuild_message;
  public static String browserRebuild_DirectoryExists_title;
  public static String browserRebuild_DirectoryExists_message;

  public static String scionServerFlavor_title;
  public static String scionServerFlavor_stdstream_label;
  public static String scionServerFlavor_network_label;

  public static String scionVerboseInteraction_title;
  public static String browserVerboseInteraction_title;

  public static String scionVersionMismatch_title;
  public static String scionVersionMismatch_message;

  public static String scionServerDoesntExist_title;
  public static String scionServerDoesntExist_message;

  public static String buildwrapper_preferences_label;
  public static String buildwrapperDoesntExist_title;
  public static String buildwrapperDoesntExist_message;

  public static String hlint_preferences_label;

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

  public static String scionBrowserNoDatabaseLoaded;
  public static String scionBrowserNoDatabaseLoadedOrHoogleNotPresent;

  public static String scionServerInstallError;
  public static String scionServerInstallFailed;
  public static String browserServerInstallError;
  public static String browserServerInstallFailed;
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

  public static String newSnapProjectWizard_pageDesc;
  public static String newSnapProjectWizard_pageTitle;
  public static String newSnapProjectWizard_windowTitle;
  public static String newSnapProjectWizard_error;
  public static String newSnapProjectWizard_error_title;
  public static String newSnapProjectWizard_error_message;
  public static String newSnapProjectWizard_job;


  public static String newYesodProjectWizard_pageDesc;
  public static String newYesodProjectWizard_pageTitle;
  public static String newYesodProjectWizard_windowTitle;
  public static String newYesodProjectWizard_error;
  public static String newYesodProjectWizard_error_title;
  public static String newYesodProjectWizard_error_message;
  public static String newYesodProjectWizard_job;

  public static String newGtkProjectWizard_pageDesc;
  public static String newGtkProjectWizard_pageTitle;
  public static String newGtkProjectWizard_windowTitle;

  public static String newProjectWizardPage_NameGroup_label_text;
  public static String newProjectWizardPage_Message_enterProjectName;
  public static String newProjectWizardPage_Message_projectAlreadyExists;
  public static String newProjectWizardPage_Message_projectInvalidName;
  public static String newProjectWizardPage_Message_projectNonAsciiName;
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
  public static String newProjectWizardPage_Message_alreadyExists;

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
  public static String renameProcessor_newname;
  public static String renameProcessor_newname_empty;
  public static String renameProcessor_scope;
  public static String renameProcessor_scope_project;
  public static String renameProcessor_scope_workspace;
  public static String renameProcessor_empty;

  public static String editor_textHover_error;
  public static String editor_actions_source;
  public static String editor_occurrences_job;

  public static String scion_console_title;
  public static String scion_preferences_title;
  public static String scion_delta_error;

  public static String bw_console_title;

  public static String libraries_description;
  public static String libraries_add_title;
  public static String libraries_add_available_title;
  public static String libraries_add_component_title;
  public static String libraries_add_component_all;
  public static String libraries_add_component_selected;
  public static String libraries_add_version_title;
  public static String none;
  public static String libraries_add_version_specific;

  public static String outline_sortByName;
  public static String outline_sortByName_tooltip;
  public static String outline_sortByName_description;

  public static String module_inclusion_title;
  public static String module_inclusion_field_section;
  public static String module_inclusion_field_include;
  public static String module_inclusion_field_expose;
  public static String module_inclusion_field_editor;
  public static String module_inclusion_page_title;
  public static String module_inclusion_page_description;
  public static String module_inclusion_error;
  public static String module_inclusion_nosourcefolder;

  public static String quickfix_marker_annotation_name;
  public static String resolve_missingtype;
  public static String resolve_addpragma;
  public static String resolve_import_remove;
  public static String resolve_import_replace;
  public static String resolve_import_element_replace;
  public static String resolve_addpackage;
  public static String resolve_addoption;
  public static String resolve_import_add;
  public static String resolve_import_remove_part;
  public static String resolve_hlint;
  public static String resolve_hlint_explain;
  public static String resolve_hlint_remove;
  public static String resolve_hlint_replace;
  public static String resolve_install_one;
  public static String resolve_install_all;

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

  public static String preferences_editor_contentass;
  public static String preferences_editor_contentass_scope;
  public static String preferences_editor_contentass_scope_imported;
  public static String preferences_editor_contentass_scope_project;
  public static String preferences_editor_contentass_scope_all;
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
  public static String preferences_editor_syntax_thread_threshold;
  public static String preferences_editor_syntax_thread_threshold_note;

  public static String preferences_editor_typing_title;
  public static String preferences_editor_typing_spaces_tabs;
  public static String preferences_editor_typing_tab_width;
  public static String preferences_editor_typing_cabal_tab_width;

  public static String preferences_debug_title;
  public static String preferences_debug_description;
  public static String preferences_debug_break_on_error;
  public static String preferences_debug_break_on_exception;
  public static String preferences_debug_print_with_show;
  public static String preferences_run_command_history_max;

  public static String preferences_search_haddock_description;
  public static String preferences_search_haddock_help;
  public static String preferences_search_haddock_new;
  public static String preferences_search_haddock_new_empty;

  public static String explorer_libraries;
  public static String explorer_libraries_noimpl;
  public static String explorer_outline_open;
  public static String explorer_outline_open_cabal;

  public static String log_nodetails;
  public static String noproject;
  public static String sharedScionInstance_console;
  public static String sharedBrowserInstance_console;

  public static String unpackScionArchive_title;
  public static String scionBuildJob_title;
  public static String unpackBrowserArchive_title;
  public static String browserBuildJob_title;
  public static String cabalUpdateJob_title;

  public static String scionArchiveResourceNotFound;
  public static String scionArchiveFileException;
  public static String scionArchiveNonspecificFileException;
  public static String scionServerProgress_title;
  public static String scionServerProgress_subtask1;
  public static String scionServerProgress_subtask2;
  public static String cabalUpdateProgress;
  public static String installExecutablesProgress;
  //public static String builWrapperInstallProgress;
  //public static String scionBrowserInstallProgress;
  public static String installExecutableProgress;
  public static String installExecutableMissing;
  public static String executablesmissing_install;
  public static String executablesmissing_user;
  public static String executablesmissing_ignore;
  public static String executablestoo_old_title;
  public static String executablestoo_old_message1;
  public static String executablestoo_old_message2;
  public static String error_getVersion;
  public static String error_checkVersion;
  public static String executables_extra;

  public static String executablesmissing_title;
  public static String executablesmissing_message1;
  public static String executablesmissing_message2;
  // public static String scionServerProgress_completed_title;
  // public static String scionServerProgress_completed_message;

  public static String browserArchiveResourceNotFound;
  public static String browserArchiveFileException;
  public static String browserArchiveNonspecificFileException;
  public static String browserServerProgress_title;
  public static String browserServerProgress_subtask1;
  public static String browserServerProgress_subtask2;

  public static String noCabalImplementation_title;
  public static String noCabalImplementation_message;
  public static String zerolenCabalExecutable_message;
  public static String noCabalImplementationForInstall_error;

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
  public static String install_dependencies_job;
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
  public static String test_error;
  public static String test_error_text;
  public static String test_version_fail;

  public static String exportDoc_job;
  public static String exportDoc_error;
  public static String exportDoc_error_text;
  public static String exportDoc_options;
  public static String exportDoc_options_hoogle;
  public static String exportDoc_options_html;
  public static String exportDoc_options_executables;
  public static String exportDoc_options_internal;
  public static String exportDoc_options_css;
  public static String exportDoc_options_css_choose;
  public static String exportDoc_options_filter_css;
  public static String exportDoc_options_filter_all;
  public static String exportDoc_options_hscolour;
  public static String exportDoc_options_hscolour_css;
  public static String exportDoc_options_hscolour_css_choose;

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
  public static String properties_extra_description;
  public static String properties_extra_libdirs;
  public static String properties_extra_incdirs;
  public static String properties_extra_free;
  public static String properties_extra_free_title;
  public static String properties_extra_free_message;

  public static String error_tabs;
  public static String error_tabs_message;

  public static String browser_definedInModules;
  public static String browser_packagedInPackages;
  public static String browser_definedInSeveralLocations;

  public static String advancedPage_title;
  public static String advancedPage_cabalSection;
  public static String advancedPage_cabalMinimalVersion;
  public static String advancedPage_cabalBuildType;
  public static String advancedPage_cabalTestedWith;
  public static String advancedPage_dataFiles;
  public static String advancedPage_selectDataFiles;

  public static String compilerChooser_title;
  public static String compilerChooser_version;

  public static String cabalEditor_add;
  public static String cabalEditor_remove;

  public static String cabalEditor_addDependency;
  public static String cabalEditor_dependencies;
  public static String cabalEditor_dependenciesPackage;
  public static String cabalEditor_dependenciesVersion;
  public static String cabalEditor_sourceDirectories;
  public static String cabalEditor_exposedModules;
  public static String cabalEditor_auxiliaryModules;
  public static String cabalEditor_mainModule;
  public static String cabalEditor_compilerOptions;
  public static String cabalEditor_compilerFlags;
  public static String cabalEditor_compilerExtensions;

  public static String cabalEditor_library;
  public static String cabalEditor_isALibrary;
  public static String cabalEditor_executables;
  public static String cabalEditor_newExecutableString;
  public static String cabalEditor_newExecutableBlankError;
  public static String cabalEditor_newExecutableAlreadyExistsError;
  public static String cabalEditor_testSuites;
  public static String cabalEditor_newTestSuiteString;
  public static String cabalEditor_newTestSuiteBlankError;
  public static String cabalEditor_newTestSuiteAlreadyExistsError;
  public static String cabalEditor_testType;

  public static String cabalEditor_modules;
  public static String cabalEditor_module;
  public static String cabalEditor_exposed_modules;
  public static String cabalEditor_other_modules;
  public static String cabalEditor_main_modules;
  public static String cabalEditor_test_modules;

  public static String cabalEditor_detailedTestSuite;
  public static String cabalEditor_stdioTestSuite;
  public static String cabalEditor_testFrameworkTestSuite;
  public static String cabalEditor_isTestFrameworkTestSuite;

  public static String renameParticipant_title;
  public static String moveParticipant_title;
  public static String copyParticipant_title;
  public static String updateReferences;
  public static String updateCabalFile;

  public static String renameFolderParticipant_title;
  public static String moveFolderParticipant_title;
  public static String copyFolderParticipant_title;

  public static String runSourceGraph_error;
  public static String runSourceGraph_errorTitle;

  public static String hoogle_downloadingData;
  public static String hoogle_dataNotPresent_title;
  public static String hoogle_dataNotPresent_message;

  public static String alex_newFile;
  public static String happy_newFile;
  public static String uuagc_newFile;

  public static String contenttype_alex_name;
  public static String contenttype_happy_name;
  public static String contenttype_uuagc_name;

  public static String new_alex;
  public static String new_happy;
  public static String new_uuagc;

  public static String creating_file;

  public static String uuagcEditor_uuagcOptions;
  public static String uuagcEditor_source;
  public static String uuagcEditor_dataDescr;
  public static String uuagcEditor_semfunsDescr;
  public static String uuagcEditor_catasDescr;
  public static String uuagcEditor_signaturesDescr;
  public static String uuagcEditor_newtypesDescr;
  public static String uuagcEditor_prettyDescr;
  public static String uuagcEditor_wrappersDescr;
  public static String uuagcEditor_renameDescr;
  public static String uuagcEditor_nestDescr;
  public static String uuagcEditor_haskellSyntaxDescr;
  public static String uuagcEditor_selfDescr;
  public static String uuagcEditor_cycleDescr;


  public static String cabalPackagesView_installed;
  public static String cabalPackagesView_all;
  public static String cabalPackagesView_filter;
  public static String cabalPackagesView_action_update;
  public static String cabalPackagesView_action_update_ok;
  public static String cabalPackagesView_action_update_error;
  public static String cabalPackagesView_action_update_running;
  public static String cabalPackagesView_action_install;
  public static String cabalPackagesView_action_install_user;
  public static String cabalPackagesView_action_install_global;
  public static String cabalPackagesView_action_install_error;
  public static String cabalPackagesView_action_install_running;
  public static String cabalPackagesView_action_install_options;
  public static String cabalPackagesView_info_running;
  public static String cabalPackagesView_info_error;
  public static String cabalPackagesView_list_running;
  public static String cabalPackagesView_list_error;
  public static String cabalPackagesView_info_installed;
  public static String cabalPackagesView_info_more;
  public static String cabalPackagesView_info_more_running;
  public static String cabalPackagesView_info_browser;
  public static String cabalPackagesView_selected;
  public static String cabalPackagesView_list;
  public static String cabalPackagesView_matching;

  public static String browser_allDatabases;
  public static String browser_localDatabase;
  public static String browser_hackageDatabase;
  public static String browser_packageDatabase;
  public static String browser_hoogleSearchIn;

  public static String NewModuleWizard_0;

  public static String NewModuleWizard_1;

  public static String NewModuleWizard_2;

  public static String NewModuleWizard_4;

  public static String NewModuleWizardPage_1;

  public static String NewModuleWizardPage_10;

  public static String NewModuleWizardPage_11;

  public static String NewModuleWizardPage_2;

  public static String NewModuleWizardPage_3;

  public static String NewModuleWizardPage_4;

  public static String NewModuleWizardPage_5;

  public static String NewModuleWizardPage_6;

  public static String NewModuleWizardPage_7;

  public static String NewModuleWizardPage_8;

  public static String NewModuleWizardPage_9;
  public static String NewModuleWizardPage_FolderIsParent;

  public static String scionBrowser_hooglePath;

  public static String Validator_0;

  public static String Validator_1;

  public static String Validator_2;

  public static String Validator_3;

  public static String Validator_4;

  public static String Validator_5;

  public static String Validator_6;

  public static String thingatpoint_none;
  public static String tasks_create_error;
  public static String tasks_pref_title;
  public static String tasks_pref_text;
  public static String tasks_pref_tag;
  public static String tasks_pref_priority;
  public static String tasks_pref_priority_high;
  public static String tasks_pref_priority_normal;
  public static String tasks_pref_priority_low;
  public static String tasks_pref_case;
  public static String tasks_pref_edit;
  public static String tasks_pref_new;

  public static String generic_new;
  public static String generic_edit;
  public static String generic_remove;

  public static String job_syntax_coloring;

  public static String OpenModuleAction_title;
  public static String OpenModuleAction_matching;
  public static String  OpenModuleAction_text;
  public static String OpenModuleAction_nomodule;
  public static String OpenModuleAction_nomodulesel;

  public static String References_result_label;
  public static String References_result_label_project;
  public static String References_result_label_project_single;
  public static String References_result_label_worskpace;
  public static String References_result_label_worskpace_single;
  public static String References_result_tooltip;
  public static String References_query_label;
  public static String References_result_refreshing;
  public static String References_result_location;


  public static String SearchPage_text;
  public static String SearchPage_type;
  public static String SearchPage_type_types;
  public static String SearchPage_type_modules;
  public static String SearchPage_type_constructors;
  public static String SearchPage_type_functions;
  public static String SearchPage_scope;
  public static String SearchPage_scope_declarations;
  public static String SearchPage_scope_references;
  public static String SearchPage_scope_all;

  public static String yesod_devel;
  public static String yesod_test;
  public static String yesod_cabaldev;

  public static String proposal_category_mode;

  private static final String BUNDLE_NAME = UITexts.class.getPackage()
      .getName() + ".uitexts"; //$NON-NLS-1$


  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}