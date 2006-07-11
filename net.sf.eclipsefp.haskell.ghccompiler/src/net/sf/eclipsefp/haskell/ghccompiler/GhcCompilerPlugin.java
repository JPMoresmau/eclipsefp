// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;


/** <p>The main plugin class to be used in the desktop.</p>
  *
  * @author the mighty PDE wizard 
  */
public class GhcCompilerPlugin extends AbstractUIPlugin {

  // The shared instance.
  private static GhcCompilerPlugin plugin;
  
  public GhcCompilerPlugin() {
    plugin = this;
  }
  
  /** <p>returns the shared instance.</p> */
  public static GhcCompilerPlugin getDefault() {
    return plugin;
  }
  
  /** <p>returns the unique identifier for the GhcCompilerPlugin.</p> */
  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }

  public static boolean isTracing() {
    String option = getPluginId() + "/trace/compiling";
    String value = Platform.getDebugOption( option );
    return value != null && value.equals( "true" );
  }
  

  // logging
  //////////
  
  public static void log( final String message, final int severity ) {
    Status status = new Status( severity, 
                                getPluginId(), 
                                IStatus.OK, 
                                message, 
                                null );
    getDefault().getLog().log( status );
  }

  public static void log( final String message, final Throwable throwable ) {
    Status status = new Status( IStatus.ERROR, 
                                getPluginId(), 
                                IStatus.OK, 
                                message, 
                                throwable );
    getDefault().getLog().log( status );
  } 
}