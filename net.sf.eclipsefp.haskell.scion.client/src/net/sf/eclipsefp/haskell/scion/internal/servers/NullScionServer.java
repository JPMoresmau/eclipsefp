package net.sf.eclipsefp.haskell.scion.internal.servers;

import java.io.File;
import java.io.Writer;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

public class NullScionServer extends ScionServer {
  public NullScionServer(IProject project, IPath serverExecutable, Writer serverOutput, File directory) {
    super(project, serverExecutable, serverOutput, directory);
  }
  
  @Override
  public boolean sendCommand(ScionCommand command) {
    return true;
  }
  
  @Override
  public boolean queueCommand(ScionCommand command) {
    return true;
  }
}
