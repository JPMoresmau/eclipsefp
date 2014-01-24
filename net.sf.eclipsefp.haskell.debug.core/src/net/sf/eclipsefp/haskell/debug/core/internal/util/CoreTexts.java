// Copyright (c) 2006-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.core.internal.util;

import org.eclipse.osgi.util.NLS;

/** <p>provides internationalized String messages for the core.</p>
  *
  * @author Leif Frenzel
  */
public final class CoreTexts extends NLS {

  // message fields
  public static String haskellLaunchDelegate_noExe;
  public static String haskellLaunchDelegate_noProcess;

  public static String commandonchange_failed;
  public static String console_command_failed;

  public static String breakpoint_message;
  public static String thread_default_name;

  public static String launchconfiguration_delete_failed;

  public static String running;
  public static String testSuite_waiting;
  public static String profiling_waiting;

  public static String jdt_notFound_title;
  public static String jdt_notFound_message;


  public static String duration_hour;
  public static String duration_hours;
  public static String duration_minute;
  public static String duration_minutes;
  public static String duration_second;
  public static String duration_seconds;
  public static String duration_milliseconds;
  public static String duration_sep;

  private static final String BUNDLE_NAME
    = CoreTexts.class.getPackage().getName() + ".coretexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, CoreTexts.class );
  }

  /**
   * format duration in current locale
   * @param duration
   * @return
   */
  public static String formatDuration(long duration){
    long ms=duration%1000;
    String ret=NLS.bind(duration_milliseconds,ms);
    duration/=1000;
    long s=duration%60;
    if (s>0){
      String sS=null;
      if (s==1){
        sS=NLS.bind(duration_second,s);
      } else {
        sS=NLS.bind(duration_seconds,s);
      }
      ret=NLS.bind( duration_sep, sS,ret );
      duration/=60;
      long m=duration%60;
      if (m>0){
        String mS=null;
        if (m==1){
          mS=NLS.bind(duration_minute,m);
        } else {
          mS=NLS.bind(duration_minutes,m);
        }
        ret=NLS.bind( duration_sep, mS,ret );
        long h=duration/60;

        if (h>0){
          String hS=null;
          if (h==1){
            hS=NLS.bind(duration_hour,h);
          } else {
            hS=NLS.bind(duration_hours,h);
          }
          ret=NLS.bind( duration_sep, hS,ret );
        }
      }
    }

    return ret;
  }

}