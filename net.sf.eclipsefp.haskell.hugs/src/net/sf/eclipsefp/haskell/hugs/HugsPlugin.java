// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs;

import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/** <p>The main plugin class to be used in the desktop.</p>
  *
  * @author The mighty PDE wizard 
  */
public class HugsPlugin extends AbstractUIPlugin {

  //The shared instance.
  private static HugsPlugin plugin;

  public HugsPlugin() {
    super();
    plugin = this;
  }

  public static HugsPlugin getDefault() {
    return plugin;
  }

  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }
  
  
  // logging and tracing
  //////////////////////
  
  public static void log( final String message, final Throwable throwable ) {
    Status status = new Status( IStatus.ERROR, 
                                getPluginId(), 
                                IStatus.OK, 
                                message, 
                                throwable );
    getDefault().getLog().log( status );
  }
  
  public static boolean isTracing() {
    String option = getPluginId() + "/trace/calls";
    String value = Platform.getDebugOption( option );
    return value != null && value.equals( "true" );
  }
}