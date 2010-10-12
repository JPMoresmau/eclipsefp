package net.sf.eclipsefp.haskell.scion.client;

import java.io.File;
import java.io.Writer;

import net.sf.eclipsefp.haskell.scion.internal.client.BuiltInScionServer;

import org.eclipse.core.resources.IProject;

/** The built-in scion server factory. This is really a special case of the user-specified
 * server, where the path to the executable is well known.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class BuiltInServerFactory implements IScionServerFactory {
  /** Container class for the {@link BuiltInServerFactory BuiltInServerFactory} singleton
   * instance.
   */
  private final static class SingletonContainer {
    private final static BuiltInServerFactory theInstance = new BuiltInServerFactory();
  }
  
  /** Default constructor. This is hidden so preserve singleton semantics. */
  private BuiltInServerFactory() {
    // NOP
  }
  
  /** Get the singleton factory instance */
  public final static BuiltInServerFactory getDefault() {
    return SingletonContainer.theInstance;
  }

  /** Generate a new BuiltInServer instance */
  public IScionServer createScionServer(IProject project, Writer outStream) {
    File directory = (project !=null) ? new File(project.getLocation().toOSString()) : null;
    return new BuiltInScionServer(outStream, directory);
  }
}
