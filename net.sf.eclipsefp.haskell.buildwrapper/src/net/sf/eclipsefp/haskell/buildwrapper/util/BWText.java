package net.sf.eclipsefp.haskell.buildwrapper.util;

import org.eclipse.osgi.util.NLS;

/**
 * Provides access to the internationalized UI texts.
 * 
 * @author Thomas ten Cate
 */
public class BWText extends NLS {
  public static String        process_launch_error;
  public static String        process_parse_error;
  
  public static String        process_parse_note_error;
  public static String        process_apply_note_error;
  
  public static String        job_build;
  public static String        job_synchronize;
  
  public static String        error_deleteMarkers;
  
  private static final String BUNDLE_NAME = BWText.class.getPackage().getName() + ".bwtext"; //$NON-NLS-1$

  static {
    NLS.initializeMessages(BUNDLE_NAME, BWText.class);
  }
}
