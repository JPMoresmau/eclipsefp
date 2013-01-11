package net.sf.eclipsefp.haskell.buildwrapper.util;

import org.eclipse.osgi.util.NLS;

/**
 * Provides access to the internationalized UI texts.
 * 
 * @author JP Moresmau
 */
public class BWText extends NLS {
  public static String        process_launch_error;
  public static String        process_parse_error;
  
  public static String        process_parse_note_error;
  public static String        process_apply_note_error;
  
  public static String        process_parse_component_error;
  public static String        process_parse_package_error;
  public static String        process_parse_path_error;
  public static String		  process_parse_outline_error;
  public static String 		  process_parse_thingatpoint_error;
  public static String		  process_parse_import_clean_error;
  
  public static String        project_members_list_error;
  public static String        project_cabal_duplicate;
  
  public static String        job_build;
  public static String        job_clean;
  public static String        job_synchronize;
  public static String        job_components;
  public static String        job_dependencies;
  public static String        job_import_clean;
  
  public static String        error_deleteMarkers;
  public static String        error_gettingFlags;
  public static String		  error_noexe;
  public static String		  error_derived;
  public static String        error_refreshLocal;
  public static String 		  error_clean;
  public static String 		  error_parsing_usage_path;
  public static String 		  error_parsing_usage_file;
  public static String 		  error_setup_db;
  public static String 		  error_db;
  public static String 		  error_no_db;
  
  public static String 		  outline_job_name;
  public static String 		  editor_job_name;
  public static String 		  occurrences_job_name;
  public static String 		  thingatpoint_job_name;

  
  private static final String BUNDLE_NAME = BWText.class.getPackage().getName() + ".bwtext"; //$NON-NLS-1$

  static {
    NLS.initializeMessages(BUNDLE_NAME, BWText.class);
  }
}
