package net.sf.eclipsefp.haskell.scion.internal.servers;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.runtime.IPath;

/**
 * Representation of the Scion server on the Java side.
 * 
 * @author Thomas ten Cate
 */
public class NetworkScionServer extends GenericScionServer {
  /** Number of times to try to connect or relaunch an operation on timeout */
  private static final int           MAX_RETRIES        = 5;
  /** Accept thread initial timeout, 1/2 second in milliseconds */
  private static final int           ACCEPT_INITIAL_TMO = 1000 / 2;

  private static final AtomicInteger threadNb           = new AtomicInteger(1);
  private static final AtomicInteger acceptNb           = new AtomicInteger(1);

  private Socket                     socket;

  private Thread                     serverOutputThread;

  public NetworkScionServer(IPath serverExecutable, Writer serverOutput, File directory) {
    super(serverExecutable, serverOutput, directory);
  }

  /**
   * Starts the Scion server.
   */
  @Override
  protected synchronized void doStartServer(String projectName) throws ScionServerStartupException {
    startServerProcess();
    serverOutputThread = new Thread(CLASS_PREFIX + (threadNb.getAndIncrement())) {
      public void run() {
        while (serverOutStream != null) {
          slurpServerOutput();
          try {
            Thread.sleep(100);
          } catch (InterruptedException ie) {
            // noop
          }
        }
      }
    };
    serverOutputThread.setDaemon(true);
    serverOutputThread.start();
    // slurpServerOutput();
  }

  /**
   * Stops the Scion server under all circumstances.
   */
  public synchronized void doStopServer() {
    stopServerProcess();
  }

  // //////////////////////////
  // Server process handling

  /**
   * Launches the external Scion server process.
   * 
   * Returns immediately if the process has been launched successfully; throws
   * an exception otherwise.
   * 
   * @throws ScionServerStartupException
   *           if the server could not be started; the inner exception will give
   *           detailed information
   */
  private void startServerProcess() throws ScionServerStartupException {
    Trace.trace(CLASS_PREFIX, "Starting server");
    try {
      final ServerSocket bindSock = new ServerSocket();
      bindSock.bind(null, 1);
      ScionPlugin.logInfo("== bindSock = ".concat(bindSock.getLocalSocketAddress().toString()));

      AcceptThread acceptJob = new AcceptThread("scionServer-accept" + acceptNb.getAndIncrement(), this, bindSock);

      acceptJob.setDaemon(true);
      acceptJob.start();

      // Construct the command line
      List<String> command = new LinkedList<String>();
      command.add(serverExecutable.toOSString());
      command.add("-c");
      command.add("-p");
      command.add(String.valueOf(bindSock.getLocalPort()));

      // Launch the process
      ProcessBuilder builder = new ProcessBuilder(command);
      if (directory != null && directory.exists()) {
        builder.directory(directory);
      }
      builder.redirectErrorStream(true); // send server's stderr to its stdout
      try {
        process = builder.start();
      } catch (Throwable ex) {
        throw new ScionServerStartupException(ScionText.scionServerCouldNotStart_message, ex);
      }

      // Connect to the process's stdout to capture messages
      /* try {
      } catch (UnsupportedEncodingException ex) {
        // make compiler happy, because UTF8 is always supported
      } */
      Trace.trace(CLASS_PREFIX, "Server launched.");

      int retries = 0;
      synchronized (this) {
        int tmo = ACCEPT_INITIAL_TMO;

        while (retries < MAX_RETRIES && socket == null) {
          try {
            this.wait(tmo);
            tmo *= 2;
            retries += 1;
          } catch (InterruptedException e) {
            // Ignore the interruption and continue.
          }
        }
      }

      if (retries >= MAX_RETRIES || socket == null) {
        ScionPlugin.logInfo("== Accept thread did not receive connection, terminating.");
        acceptJob.setTerminate();
        try {
          acceptJob.join();
          ScionPlugin.logInfo("== Accept thread terminated.");
          throw new ScionServerStartupException(ScionText.scionServerCouldNotStart_message);
        } catch (InterruptedException e) {
          // Don't care...
        }
      }

      serverOutStream = new BufferedReader(new InputStreamReader(socket.getInputStream(), "UTF8"));
      serverInStream = new BufferedWriter (new OutputStreamWriter(socket.getOutputStream(), "UTF8"));

      // Don't need the bind socket any more...
      bindSock.close();
    } catch (UnsupportedEncodingException ex) {
      // Should not happen, but it does make the compiler happy.
    } catch (IOException e) {
      // Should not fail, since we're asking for an ephemeral port, but report
      // it nonetheless
      throw new ScionServerStartupException(ScionText.scionServerCouldNotStart_message, e);
    }
  }

  /**
   * Stops the server (if running) and frees up resources. It is allowed to call
   * this method in any state.
   */
  private void stopServerProcess() {
    if (socket != null) {
      try {
        socket.close();
        socket = null;
      } catch (Throwable ex) {
        // ignore
      }
    }
  }

  /**
   * Reads from the server's stdout until EOF is reached. Useful to collect
   * information after the server process has unexpectedly died.
   * 
   * Handles all exceptions internally.
   * 
   * @return all data readily available on the server's stdout. Returns
   *         <code>null</code> if there is no stdout to read from.
   */
  private String lastWords() {
    // Collect a post-mortem by reading all that's left in the server's output
    // stream
    if (serverOutStream == null)
      return null;
    StringBuffer lastWords = new StringBuffer();
    try {
      while (serverHasOutput()) {
        char[] buf = new char[1024];
        int nread = serverOutStream.read(buf);
        
        if (nread > 0) {
          String line = new String(buf, 0, nread);
          if (lastWords.length() > 0) {
            lastWords.append(PlatformUtil.NL);
          }
          lastWords.append(line);
        }
      }
    } catch (IOException ex) {
      // ignore
    }
    return lastWords.toString().trim();
  }

  // //////////////////////////////
  // Server stdout communication

  /**
   * Reads from the server's stdout (and stderr) and sends its output to the
   * tracer (if tracing). If there is no output ready to be read, this returns
   * immediately.
   * 
   * Errors while reading are silently ignored.
   */
  private void slurpServerOutput() {
    try {
      while (serverHasOutput()) {
        char[] buf = new char[1024];
        serverOutStream.read(buf);
      }
    } catch (IOException ex) {
      // too bad, ignore
    }
  }

  private boolean serverHasOutput() throws IOException {
    return (serverOutStream != null && serverOutStream.ready());
  }

  private class AcceptThread extends Thread {
    private NetworkScionServer instance;
    private ServerSocket       bindSock;
    private boolean            terminateFlag;

    public AcceptThread(String name, NetworkScionServer instance, ServerSocket bindSock) {
      super(name);
      this.instance = instance;
      this.bindSock = bindSock;
      this.terminateFlag = false;
    }

    public void run() {
      int timeout = 1000 / 2;
      try {
        bindSock.setSoTimeout(timeout);
        while (!terminateFlag) {
          socket = bindSock.accept();
          synchronized (instance) {
            instance.notifyAll();
          }
        }
      } catch (SocketException e) {
        // Yeah, yeah, accept timed out...
      } catch (IOException e) {
        // Retry accepting connections...
      }
    }

    public void setTerminate() {
      terminateFlag = true;
    }
  }
}
