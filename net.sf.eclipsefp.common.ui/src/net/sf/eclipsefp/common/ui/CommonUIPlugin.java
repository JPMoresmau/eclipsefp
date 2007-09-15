// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.*;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/** <p>The main plugin class for the commons ui plugin.</p>
  * 
  * @author The mighty PDE wizard 
  */
public class CommonUIPlugin extends AbstractUIPlugin {

  public static final String EXT_POINT_CONFIGURATOR_PAGES = "configuratorPages";
  
  private static CommonUIPlugin plugin;

  /** <p>constructs the singleton instance of the commons ui plugin.
   * Note: This constructir is used by the Eclipse runtime and must not
   * be called by clients.</p> */
  public CommonUIPlugin() {
    plugin = this;
  }
  
  /** <p>returns the singleton instance of the commons ui plugin.</p> */
  public static CommonUIPlugin getDefault() {
    return plugin;
  }

  /** <p>returns the plugin id of the commons ui plugin.</p> */
  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }  
  
  
  // logging
  //////////
  
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
  public static void log( final String message, final Throwable thr ) {
    log( new Status( IStatus.ERROR, 
                     getPluginId(), 
                     IStatus.ERROR, 
                     message, 
                     thr ) );
  }
  
  
  // UI helping methods
  /////////////////////

  /** <p>returns a reference to the currently active page in the 
    * workbench. This is a convenience method for the call to the
    * platform ui to return the active page.</p> */
  public static IWorkbenchPage getActiveWorkbenchPage() {
    IWorkbenchPage result = null;
    IWorkbenchWindow window = getActiveWindow();
    if( window != null ) {
      result = window.getActivePage();
    }
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private static IWorkbenchWindow getActiveWindow() {
    return PlatformUI.getWorkbench().getActiveWorkbenchWindow();
  }
}