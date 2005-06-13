// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.BundleContext;

/** The main plugin class for the native parser bridge.
  * 
  * @author The mighty PDE wizard
  */
public class ParserPlugin extends Plugin {

  //The shared instance.
  private static ParserPlugin plugin;

  public ParserPlugin() {
    super();
    plugin = this;
  }

  public static ParserPlugin getDefault() {
    return plugin;
  }

  
  // interface methods of Plugin
  //////////////////////////////
  
  public void stop( final BundleContext context ) throws Exception {
    super.stop( context );
    NativeParser.getInstance().dispose();
  }
  
  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }
  
  
  // logging and tracing
  //////////////////////
  
  public static void log( final String message, final Throwable thr ) {
    String id = getDefault().getBundle().getSymbolicName();
    Status status = new Status( IStatus.ERROR, id, IStatus.OK, message, thr );
    getDefault().getLog().log( status );
  }
  
  public static boolean isTracing() {
    String option =   getDefault().getBundle().getSymbolicName() 
                    + "/trace/unmarshal";
    String value = Platform.getDebugOption( option );
    return value != null && value.equals( "true" );
  }
}
