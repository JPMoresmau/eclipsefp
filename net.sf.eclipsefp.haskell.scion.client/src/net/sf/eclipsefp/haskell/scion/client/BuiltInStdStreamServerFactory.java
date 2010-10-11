package net.sf.eclipsefp.haskell.scion.client;

import java.io.File;
import java.io.Writer;

import net.sf.eclipsefp.haskell.scion.internal.client.StdStreamScionServer;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

/** The built-in scion server factory. This is really a special case of the user-specified
 * server, where the path to the executable is well known.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class BuiltInStdStreamServerFactory implements IScionServerFactory {
  /** Default constructor. */
  public BuiltInStdStreamServerFactory() {
    // NOP
  }

  /** Generate a new BuiltInServer instance */
  public IScionServer createScionServer(IProject project, Writer outStream) {
    File directory = (project !=null) ? new File(project.getLocation().toOSString()) : null;
    return new StdStreamScionServer(ScionPlugin.builtinServerExecutablePath(), outStream, directory);
  }

  public IPath getServerExecutable() {
    return ScionPlugin.builtinServerExecutablePath();
  }
}
