// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.core;

import org.eclipse.core.runtime.*;

/** <p>The main plugin class for the commons core plugin.</p>
  * 
  * @author Leif Frenzel
  */
public class CommonCorePlugin extends Plugin {

  private static CommonCorePlugin plugin;

  /** <p>constructs the singleton instance of the commons core plugin.
    * Note: This constructir is used by the Eclipse runtime and must not
    * be called by clients.</p> */
  public CommonCorePlugin() {
    plugin = this;
  }

  /** <p>returns the singleton instance of the commons core plugin.</p> */
  public static CommonCorePlugin getDefault() {
    return plugin;
  }

  /** <p>logs the specified status object.</p> */
  public static void log( final IStatus status ) {
    getDefault().getLog().log( status );
  }

  /** <p>logs the specified message.</p> */
  public static void log( final String message ) {
    log( new Status( IStatus.ERROR, 
                     getPluginId(), 
                     IStatus.ERROR, 
                     message, 
                     null ) );
  }

  /** <p>logs the specified throwable.</p> */
  public static void log( final Throwable thr ) {
    log( new Status( IStatus.ERROR, 
                     getPluginId(), 
                     IStatus.ERROR, 
                     "Internal Error", 
                     thr ) );
  }
  
  
  // helping methods
  //////////////////
  
  private static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }
}