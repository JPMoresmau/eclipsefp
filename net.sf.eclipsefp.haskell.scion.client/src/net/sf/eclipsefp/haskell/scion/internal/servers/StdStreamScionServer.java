package net.sf.eclipsefp.haskell.scion.internal.servers;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;

/**
 * Implementation of {@link GenericScionServer GenericScionServer} using
 * standard in/out to communicate with Scion
 * 
 * @author JP Moresmau
 */
public class StdStreamScionServer extends GenericScionServer {
  private StdErrorReader stdErrorLogger;

  public StdStreamScionServer(IPath serverExecutable, Writer serverOutput, File directory) {
    super(serverExecutable, serverOutput, directory);
  }

  protected synchronized void doStartServer(String projectName) throws ScionServerStartupException {
    List<String> command = new LinkedList<String>();
    command.add(serverExecutable.toOSString());
    command.add("-i ");

    // Launch the process
    ProcessBuilder builder = new ProcessBuilder(command);
    if (directory != null && directory.exists()) {
      builder.directory(directory);
    }
    builder.redirectErrorStream(false);
    try {
      process = builder.start();
    } catch (Throwable ex) {
      throw new ScionServerStartupException(ScionText.scionServerCouldNotStart_message, ex);
    }

    // Start up the standard error reader, stall until the first character is read
    // (we know that the server is active at that point)
    stdErrorLogger = new StdErrorReader(projectName);
    stdErrorLogger.setDaemon(true);
    stdErrorLogger.start();
    
    synchronized (stdErrorLogger) {
      while (stdErrorLogger.isWaiting()) {
        try {
          stdErrorLogger.wait();
        } catch (InterruptedException ex) {
          // Keep going
        }
      }
    }

    // Connect to the process's stdout to capture messages
    // Assume that status messages and such will be UTF8 only
    try {
      serverOutStream = new BufferedReader(new InputStreamReader(process.getInputStream(), "UTF8"));
      serverInStream = new BufferedWriter(new OutputStreamWriter(process.getOutputStream(), "UTF8"));
    } catch (UnsupportedEncodingException ex) {
      // make compiler happy, because UTF8 is always supported
    }
  }
  
  @Override
  public void doStopServer() {
    if (stdErrorLogger != null) {
      synchronized (stdErrorLogger) {
        stdErrorLogger.setTerminate();
        stdErrorLogger.interrupt();
        try {
          stdErrorLogger.join();
        } catch (InterruptedException e) {
          // Don't care.
        }
        
        stdErrorLogger = null;
      }
    }
  }
  
  class StdErrorReader extends Thread {
    /** The reader's current state of wellbeing. */
    private int liveness;
    /** Termination flag */
    private boolean terminate = false;
    /** The reader is waiting for the first character */
    final static int WAITING = 0;
    /** It's alive! It's ALIVE */
    final static int ALIVE = 1;
    final static int ERROR = 2;
    
    public StdErrorReader(String threadName) {
      super("StdErrorReader [" + threadName + "]");
      liveness = WAITING;
    }
    
    public boolean isWaiting() {
      return (liveness == WAITING);
    }
    
    public void setTerminate() {
      terminate = true;
    }
    
    @Override
    public void run() {
      BufferedReader errStream = new BufferedReader( new InputStreamReader(process.getErrorStream()));
      int nread;

      do {
        try {
          nread = errStream.read();
        } catch (IOException ex) {
          nread = -1;
          try {
            errStream.close();
          } catch (IOException ex2) {
            // Ignore it, can't do anything about it.
          }
          errStream = null;
          break;
        }
      } while (nread <= 0);
      
      if (nread > 0) {
        // Up and active!
        synchronized (StdErrorReader.this) {
          liveness = ALIVE;
          StdErrorReader.this.notifyAll();
        }
        
        try {
          synchronized (serverOutput) {
            serverOutput.write(nread);
          }
          
          while (!terminate && errStream != null) {
            try {
              if (errStream.ready()) {
                String line = errStream.readLine();
                if (line != null) {
                  synchronized (serverOutput) {
                    serverOutput.write(line + PlatformUtil.NL);
                    serverOutput.flush();
                  }
                } else {
                  // Looks like we hit EOF...
                  errStream.close();
                  errStream = null;
                }
              }
            } catch (IOException ex) {
              try {
                errStream.close();
              } catch (IOException ex2) {
                // Not much we can do about this at this point.
              }
              errStream = null;
            }
          }
        } catch (IOException e) {
          try {
            errStream.close();
          } catch (IOException e2) {
            liveness = ERROR;
          }
        }
      } else {
        // Didn't get past the first character...
        try {
          errStream.close();
        } catch (IOException e) {
          // Ignore.
        }
        synchronized (StdErrorReader.this) {
          liveness = ERROR;
          errStream = null;
          StdErrorReader.this.notifyAll();
        }
      }
    }
  }
}
