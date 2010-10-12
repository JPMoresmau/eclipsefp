package net.sf.eclipsefp.haskell.scion.internal.client;

import org.eclipse.core.runtime.IProgressMonitor;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

public class NullScionServer implements IScionServer {
  
  private final static class SingletonContainer {
    private static final NullScionServer theInstance = new NullScionServer();
  }

  private NullScionServer() {
    // Schultz: "I hear nossink!"
  }
  
  public static IScionServer getDefault() {
    return SingletonContainer.theInstance;
  }
  
  public void startServer() throws ScionServerStartupException {
    // Schultz: "I see nossink!"
  }

  public void stopServer() {
    // Schultz: "I know nossink!"
  }

  public void runCommandSync(ScionCommand command, IProgressMonitor monitor) throws ScionServerException, ScionCommandException {
    // Does absolutely nothing.
  }
}
