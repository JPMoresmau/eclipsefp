package net.sf.eclipsefp.haskell.scion.internal.client;

import java.io.File;
import java.io.Writer;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

public class UserScionServer implements IScionServer {
  /** The user-specified executable */
  private IPath userExecutablePath;
  /** Project directory */
  private File directory;
  /** The output stream where server output is directed. */
  private Writer outStream;
  /** The actual server */
  AbstractScionServer server;
  
  public UserScionServer(final IPath userExecutablePath, final Writer outStream, final File directory) {
    this.userExecutablePath = userExecutablePath;
    this.directory = directory;
    this.outStream = outStream;
    this.server = null;
  }
  
  public void startServer() throws ScionServerStartupException {
      server = new StdStreamScionServer(userExecutablePath.toOSString(), outStream, directory);
              //new NetworkScionServer(serverExecutable,serverOutput,directory);

  }

  public void stopServer() {
    if (server != null) {
    }
  }

  public void runCommandSync(ScionCommand command, IProgressMonitor monitor) throws ScionServerException, ScionCommandException {
    // TODO Auto-generated method stub

  }

}
