package net.sf.eclipsefp.haskell.scion.client;

import java.io.File;
import java.io.Writer;

import net.sf.eclipsefp.haskell.scion.internal.client.UserScionServer;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

public class UserScionServerFactory implements IScionServerFactory {
  /** The path to the user's executable */
  private IPath userExecutablePath;
  
  /* Constructor
   * 
   * @param userExecutablePath An IPath to the user's executable.
   */
  public UserScionServerFactory(final IPath userExecutablePath) {
    this.userExecutablePath = userExecutablePath;
  }
  
  /** Create a new user-specified scion server */
  public IScionServer createScionServer(final IProject project, final Writer outStream) {
    File directory = (project !=null) ? new File(project.getLocation().toOSString()) : null;
    return new UserScionServer(userExecutablePath, outStream, directory); 
  }

}
