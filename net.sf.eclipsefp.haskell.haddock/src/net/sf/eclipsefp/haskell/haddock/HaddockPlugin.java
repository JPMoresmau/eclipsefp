// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/** <p>The main plugin class for the Haddock support.</p>
  * 
  * @author The mighty PDE wizard 
  */
public class HaddockPlugin extends AbstractUIPlugin {

  //The shared instance.
  private static HaddockPlugin plugin;

  public HaddockPlugin() {
    super();
    plugin = this;
  }

  public static HaddockPlugin getDefault() {
    return plugin;
  }
  
  /** <p>returns the unique identifier for the HaddockPlugin.</p> */
  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }

  public static boolean isTracing() {
    String option = getPluginId() + "/trace/commandline";
    String value = Platform.getDebugOption( option );
    return value != null && value.equals( "true" );
  }

  public static ImageDescriptor getBanner() {
    return imageDescriptorFromPlugin( getPluginId(), 
                                      "icons/full/wizban/haddockexport.gif" );
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
