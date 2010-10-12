package net.sf.eclipsefp.haskell.scion.internal.client;

import java.io.File;
import java.io.Writer;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

import org.eclipse.core.resources.IProject;


public class BuiltInScionServer extends UserScionServer {

  public BuiltInScionServer(final Writer outStream, final File directory) {
    super(ScionPlugin.builtinServerExecutablePath(), outStream, directory);
  }
  public IScionServer createScionServer(final IProject project, final Writer outStream) {
    // TODO Auto-generated method stub
    return null;
  }

}
