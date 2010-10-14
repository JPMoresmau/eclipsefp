package net.sf.eclipsefp.haskell.scion.internal.servers;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.concurrent.atomic.AtomicInteger;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Display;

/**
 * some helper code to implement a IScionServer
 * 
 * @author JP Moresmau
 * 
 */
public abstract class AbstractScionServer implements IScionServer {
  protected static final String CLASS_PREFIX         = "[ScionServer]";
  protected static final String SERVER_STDOUT_PREFIX = "[scion-server]";
  /**
   * Path to the server executable that is started and with whom EclipseFP
   * communicates
   */
  protected IPath               serverExecutable;
  /** Server logging output stream, generally tends to be a Eclipse console */
  protected Writer              serverOutput;
  /** Working directory where the server operates, can be null if no project. */
  protected File                directory;
  /** The scion-server process */
  protected Process             process;
  /** Server's standard output */
  protected BufferedReader      serverStdOutReader;
  /** Request identifier */
  private final AtomicInteger              nextSequenceNumber = new AtomicInteger(1);

  /**
   * The constructor
   * 
   * @param serverExecutable
   *          The scion-server executable
   * @param serverOutput
   *          The scion-server's logging stream
   * @param directory
   *          The scion-server's working directory
   */
  public AbstractScionServer(IPath serverExecutable, Writer serverOutput, File directory) {
    this.serverExecutable = serverExecutable;
    this.serverOutput = serverOutput;
    this.directory = directory;
  }

  /**
   * Check the server's protocol version.
   */
  public void checkProtocol(IScionCommandRunner cmdRunner) {
    ConnectionInfoCommand command = new ConnectionInfoCommand(cmdRunner, this);
    command.addJobChangeListener(new JobChangeAdapter() {
      @Override
      public void done(IJobChangeEvent event) {
        if (event.getResult().isOK()) {
          ConnectionInfoCommand command = (ConnectionInfoCommand) event.getJob();
          if (command.getVersion() != ScionPlugin.PROTOCOL_VERSION) {
            ScionPlugin.logWarning(
                NLS.bind(UITexts.scionVersionMismatch_warning, Integer.toString(command.getVersion()),
                    Integer.toString(ScionPlugin.PROTOCOL_VERSION)), null);
          }
        }
      }
    });
    command.runAsync();
  }
  
  /**
   * Reads a line from the server's stdout (or stderr), blocking until a line is
   * available. The returned line does not contain the newline character(s) at
   * the end.
   * 
   * @return The next full line of input read form the server, or null when EOF
   *         is encountered.
   */
  protected String readLineFromServer() throws IOException {
    final String line = serverStdOutReader.readLine();
    if (line != null) {
      if (serverOutput != null) {
        // serverOutput.append(line+"\n");
        // int ix=0;
        // while (line.length()-ix>1024){
        // serverOutput.append(line.substring(ix,ix+1024));
        // //serverOutput.flush();
        // ix+=1024;
        // }
        // serverOutput.append(line.substring(ix,line.length())+"\n");
        // this seems to be what works the best to write to the console
        // async in display, cut in chunks so it's not too big
        try {
          Display theDisplay = Display.getDefault();
          theDisplay.asyncExec(new Runnable() {
            public void run() {
              try {
                // serverOutput.append(line);
                // serverOutput.append("\n");
                // serverOutput.flush();
                int ix = 0;
                while (line.length() - ix > 1024) {
                  serverOutput.append(line.substring(ix, ix + 1024));
                  // serverOutput.flush();
                  ix += 1024;
                }
                serverOutput.append(line.substring(ix, line.length()) + "\n");
                serverOutput.flush();
              } catch (IOException ioe) {
                // ignore
              }
            }
          });
        } catch (SWTException e) {
          if (e.code == SWT.ERROR_THREAD_INVALID_ACCESS) {
            // UI thread is actually dead at this point, so ignore
            // (Note: This is actually semi-important on Mac OS X Cocoa.)
          } else {
            // Re-throw the exception...
            throw e;
          }
        }
      } else {
        Trace.trace(SERVER_STDOUT_PREFIX, line);
      }
    } else {
      if (serverOutput != null) {
        serverOutput.append("Server gave EOF on stdout\n");
        serverOutput.flush();
      } else {
        Trace.trace(CLASS_PREFIX, "Server gave EOF on stdout");
      }
    }
    return line;
  }

  // ISequenceNumberGenerator methods:
  
  public int nextSequenceNumber() {
    return nextSequenceNumber.getAndIncrement();
  }
}
