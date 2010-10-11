package net.sf.eclipsefp.haskell.scion.client;
import java.io.File;
import java.io.Writer;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

import net.sf.eclipsefp.haskell.scion.internal.client.NetworkScionServer;

public class NetworkStreamScionServerFactory implements IScionServerFactory {
  private IPath userExecutable;
  
  public NetworkStreamScionServerFactory(final IPath userExecutable) {
    this.userExecutable = userExecutable;
  }
  
  public IScionServer createScionServer(IProject project, Writer outStream) {
    File directory = (project !=null) ? new File(project.getLocation().toOSString()) : null;
    return new NetworkScionServer(userExecutable, outStream, directory);
  }

  public IPath getServerExecutable() {
    return userExecutable;
  }

}
