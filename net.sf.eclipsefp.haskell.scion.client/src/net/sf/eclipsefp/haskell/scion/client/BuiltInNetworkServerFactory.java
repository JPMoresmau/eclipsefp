package net.sf.eclipsefp.haskell.scion.client;

import java.io.File;
import java.io.Writer;

import net.sf.eclipsefp.haskell.scion.internal.client.NetworkScionServer;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

public class BuiltInNetworkServerFactory implements IScionServerFactory {
  /** Default constructor. */
  public BuiltInNetworkServerFactory() {
    // NOP
  }

  /** Generate a new BuiltInServer instance */
  public IScionServer createScionServer(IProject project, Writer outStream) {
    File directory = (project !=null) ? new File(project.getLocation().toOSString()) : null;
    return new NetworkScionServer(ScionPlugin.builtinServerExecutablePath(), outStream, directory);
  }

  public IPath getServerExecutable() {
    return ScionPlugin.builtinServerExecutablePath();
  }
}
