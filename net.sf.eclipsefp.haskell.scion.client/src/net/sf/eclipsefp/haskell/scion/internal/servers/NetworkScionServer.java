package net.sf.eclipsefp.haskell.scion.internal.servers;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
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
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.runtime.IPath;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

/**
 * Representation of the Scion server on the Java side.
 * 
 * @author Thomas ten Cate
 */
public class NetworkScionServer extends ScionServer {
  /** Number of times to try to connect or relaunch an operation on timeout */
  private static final int           MAX_RETRIES        = 5;
  /** Accept thread initial timeout, 1/2 second in milliseconds */
  private static final int           ACCEPT_INITIAL_TMO = 1000 / 2;
  /** */
  private static final AtomicInteger acceptNb           = new AtomicInteger(1);
  /** The socket to the scion-server */
  private Socket                     socket;
  /** I/O reader thread for process' stdout/stderr logging */
  private Thread                     serverOutputThread;
  /** The process' stdout reader stream */
  private BufferedReader             serverStdout;
  /** The input receiver Job */
  private InputReceiver              inputReceiver;

  public NetworkScionServer(String projectName, IPath serverExecutable, Writer serverOutput, File directory) {
    super(projectName, serverExecutable, serverOutput, directory);
  }

  /**
   * Starts the Scion server.
   */
  @Override
  protected synchronized void doStartServer(String projectName) throws ScionServerStartupException {
    startServerProcess();
    
    serverStdout = new BufferedReader(new InputStreamReader(process.getInputStream()));
    serverOutputThread = new Thread(getClass().getSimpleName() + "/" + projectName) {
      public void run() {
        while (serverStdout != null) {
          logServerStdout();
        }
      }
    };
    
    serverOutputThread.start();
    inputReceiver = new InputReceiver(projectName);
    inputReceiver.start();
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
    
    // Cleanup after the threads.
    try {
      serverOutputThread.interrupt();
      serverOutputThread.join();
    } catch (InterruptedException ex) {
      // Don't care.
    }
    
    try {
      inputReceiver.setTerminate();
      inputReceiver.interrupt();
      inputReceiver.join();
    } catch (InterruptedException ex) {
      // Don't care.
    }
  }

  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Server stdout/stderr communication
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  /**
   * Reads from the server's stdout (and stderr) and sends its output to the
   * tracer (if tracing). If there is no output ready to be read, this returns
   * immediately.
   * 
   * Errors while reading are silently ignored.
   */
  private void logServerStdout() {
    int nread;
    while (serverStdout != null) {
      char[] buf = new char[1024];
      try {
          nread = serverStdout.read(buf);
          if (nread > 0) {
            outputWriter.addMessage(buf, 0, nread);
          }
      } catch (IOException ex) {
        // assume the stream is closed.
        try {
          serverStdout.close();
        } catch (IOException e) {
          // Don't care.
        } finally {
          serverStdout = null;
        }
      }
    }
  }

  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Internal classes
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

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
      try {
        // 45 second socket timeout should be sufficient...
        bindSock.setSoTimeout(45 * 1000);
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

  /** The input receiver thread.
   *
   * This thread reads the scion-server's standard input and error, looking for input lines that start with
   * {@link StdStreamScionServer#PREFIX}. Matching lines are sent to {@link ScionServer#processResponse(JSONObject)}.
   *  All input is echoed to the {@link ScionServer#serverOutput} output stream.
   */
  public class InputReceiver extends Thread {
    private boolean terminateFlag;
    
    public InputReceiver(String name) {
      super(name);
      terminateFlag = false;
    }
    
    public void setTerminate() {
      terminateFlag = true;
    }

    @Override
    public void run() {
      while (!terminateFlag && serverOutStream != null) {
        JSONObject response;
        // long t0=System.currentTimeMillis();
        try {
          response = new JSONObject(new JSONTokener(serverOutStream));

          Trace.trace(FROM_SERVER_PREFIX, "%s", response.toString());
          serverOutput.write(FROM_SERVER_PREFIX.concat(response.toString()).concat(PlatformUtil.NL));
          serverOutput.flush();
          
          processResponse(response);
        } catch (JSONException ex) {
          try {
            serverOutput.write(ScionText.scionJSONParseException_message + PlatformUtil.NL);
            ex.printStackTrace(new PrintWriter(serverOutput));
            serverOutput.flush();

            stopServer();
          } catch (IOException ex2) {
            stopServer();
          }
        } catch (IOException ex) {
          // Must have caught EOF, shut down server
          stopServer();
        }
        // long t1=System.currentTimeMillis();
        // System.err.println("receive+parse:"+(t1-t0));
      }
    }
  }
}
