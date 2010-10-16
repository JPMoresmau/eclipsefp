package net.sf.eclipsefp.haskell.scion.internal.servers;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.runtime.IPath;
import org.eclipse.osgi.util.NLS;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

/**
 * some helper code to implement a GenericScionServer
 * 
 * @author JP Moresmau
 * 
 */
public abstract class GenericScionServer {
  protected static final String CLASS_PREFIX         = "[ScionServer]";
  protected static final String SERVER_STDOUT_PREFIX = "[scion-server]";
  /** Message prefix for commands send to the server */
  private static final String      TO_SERVER_PREFIX   = "[scion-server] <<";
  /** Message prefix for responses received from the server */
  private static final String      FROM_SERVER_PREFIX = "[scion-server] >>";
  /** scion-server response prefix */
  private static final String PREFIX = "scion:";
  /** The Scion protocol version number */
  private static final String      PROTOCOL_VERSION = "0.1";
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
  /** scion-server's output stream, read by EclipseFP */
  protected BufferedReader      serverOutStream;
  /** scion-server's input stream, written to by EclipseFP */
  protected BufferedWriter      serverInStream;
  /** The input receiver Job */
  private InputReceiver         inputReceiver;
  /** Request identifier */
  private final AtomicInteger   nextSequenceNumber;
  
  /** Command queue, to deal with both synchronous and asynchronous commands */
  protected final Map<Integer, RequestState> commandQueue;

  /**
   * The constructor
   * 
   * @param serverExecutable
   *          The scion-server executable
   * @param serverOutput
   *          The scion-server's logging and trace stream.
   * @param directory
   *          The scion-server's working directory
   */
  public GenericScionServer(IPath serverExecutable, Writer serverOutput, File directory) {
    this.serverExecutable = serverExecutable;
    this.serverOutput = serverOutput;
    this.directory = directory;
    
    this.process = null;
    this.serverOutStream = null;
    this.serverInStream = null;
    this.inputReceiver = null;
    this.nextSequenceNumber = new AtomicInteger(1);
    this.commandQueue =  new HashMap<Integer, RequestState>();
  }
  
  /** The default constructor. This is only used by NullScionServer */
  protected GenericScionServer() {
    this.serverExecutable = null;
    this.serverOutput = null;
    this.directory = null;
    
    this.process = null;
    this.serverOutStream = null;
    this.serverInStream = null;
    this.inputReceiver = null;
    this.nextSequenceNumber = new AtomicInteger(1);
    this.commandQueue =  new HashMap<Integer, RequestState>();
  }
 
  /** Redirect the logging stream */
  public void setOutputStream(final Writer outStream) {
    serverOutput = outStream;
  }
  
  /**
   * Start the server process.
   * 
   * @note This method should not be overridden by subclasses. Subclasses should override
   * {@link doStartServer doStartServer} instead. This is a protocol design method, where
   * {@link GenericScionServer GenericScionServer} implements code that must be executed
   * before or after the subclass' server launch.
   */
  public final void startServer(String projectName) throws ScionServerStartupException {
    Trace.trace(CLASS_PREFIX, "Starting server " + getClass().getSimpleName() + ":" + projectName);

    doStartServer(projectName);

    if (serverInStream != null) {
      String receiverName = getClass().getSimpleName() + " [" + projectName + "]";
      inputReceiver = new InputReceiver(receiverName);
      inputReceiver.start();
    }

    Trace.trace(CLASS_PREFIX, "Server started for " + getClass().getSimpleName() + ":" + projectName);
 }

  /**
   * Subclass' hook for starting up their respective server processes.
   * @param projectName TODO
   */
  protected void doStartServer(String projectName) throws ScionServerStartupException {
    // Does nothing...
    serverOutStream = null;
    serverInStream = null;
  }
  
  /**
   * Stop the server process.
   * 
   * @note This method should not be overridden by subclasses. Subclasses should override
   * {@link doStopServer doStopServer} instead. This is a protocol design method, where
   * {@link GenericScionServer GenericScionServer} implements code that must be executed
   * before or after the subclass' server launch.
   */
  public final void stopServer() {
    Trace.trace(CLASS_PREFIX, "Stopping server");

    try {
      // Stop the input receiver.
      inputReceiver.setTerminate();
      inputReceiver.interrupt();
      inputReceiver.join();

      if (serverOutStream != null) {
        serverOutStream.close();
        serverOutStream = null;
      }
      if (serverInStream != null) {
        serverInStream.close();
        serverInStream = null;
      }
      
      // Let the subclass do its thing.
      doStopServer();
      // Then kill off the server process.
      if (process != null) {
        process.destroy();
        process = null;
      }
    } catch (Throwable ex) {
      // ignore
    }

    Trace.trace(CLASS_PREFIX, "Server stopped");
  }
  
  /**
   * Subclass' hook for stopping their respective server processes.
   */
  protected void doStopServer() {
    // Base class does nothing.
  }
  
  /**
   * Send a command to the server, but do not wait for a response (asynchronous version).
   */
  public void sendCommand(ScionCommand command) {
    if (serverInStream != null) {
      sendCommand(command, new RequestState(command, false));
    }
  }
  
  /** 
   * Send a command, wait for its response.
   * 
   * @return true if the command completed successfully, otherwise false.
   */
  public final boolean sendCommandSync(ScionCommand command) {
    boolean retval = false;
    
    if (serverInStream != null) {
      RequestState reqState = new RequestState(command, true);
      synchronized (reqState) {
        sendCommand(command, reqState);
        while (reqState.status == RequestState.WAITING) {
          try {
            reqState.wait();
          } catch (InterruptedException ex) {
            // We'll spin until the request is processed...
          }
        }
      }
      retval = (reqState.status == RequestState.DONE);
    }
    return retval;
  }
  
  public final void sendCommand(ScionCommand command, RequestState reqState) {
    // Keep track of this request in the command queue
    int seqNo = nextSequenceNumber();
    command.setSequenceNumber(seqNo);
    
    synchronized ( commandQueue ) {
      commandQueue.put(new Integer(seqNo), reqState);
    }
    
    String jsonString = command.toJSONString();

    try {
      serverInStream.write(command.toJSONString() + PlatformUtil.NL);
      serverInStream.flush();
    } catch (IOException ex) {
      try {
        synchronized (serverOutput) {
          serverOutput.write(getClass().getSimpleName() + ".sendCommand encountered an exception:" + PlatformUtil.NL);
          ex.printStackTrace(new PrintWriter(serverOutput));
          serverOutput.flush();
        }
      } catch (IOException e) {
      } finally {
        stopServer();
      }
      // FIXME: Do something, like shut down the server.
    } finally {
      Trace.trace(TO_SERVER_PREFIX, "%s", jsonString);
      try {
        synchronized (serverOutput) {
          serverOutput.write(TO_SERVER_PREFIX + jsonString + PlatformUtil.NL);
          serverOutput.flush();
        }
      } catch (IOException ex) {
        // Ignore this (something creative here?) 
      }
    }
  }

  /**
   * Check the server's protocol version. This just generates a warning if the
   * version numbers do not match.
   */
  public void checkProtocol(IScionCommandRunner cmdRunner) {
    sendCommand(new ConnectionInfoCommand());
  }
  
  /**
   * Parses the given response string and stores the command result in this
   * object.
   */
  public boolean processResponse(JSONObject response) {
    boolean retval = false;
    int disposition = RequestState.DONE;
    
    if (checkResponseVersion(response)) {
      int id = 0;
      RequestState rState = null;
      ScionCommand command = null;
      
      try {
        id = response.getInt("id");
        
        synchronized (commandQueue) {
          Integer key = new Integer(id);
          rState = commandQueue.remove(key);
        }
        
        if (rState != null) {
          command = rState.command;
          Object result = response.get("result");
          try {
            command.setResponse(response);
            command.processResult(result);
            command.runSuccessors(GenericScionServer.this);
            retval = true;
          } catch (JSONException ex) {
            try {
              JSONObject error = response.getJSONObject("error");
              String name = error.getString("name");
              String message = error.getString("message");
              if (!command.onError(ex, name, message)) {
                disposition = RequestState.ERROR;
                try {
                  synchronized (serverOutput) {
                    serverOutput.write(NLS.bind(ScionText.commandError_message, name, message) + PlatformUtil.NL);
                    ex.printStackTrace(new PrintWriter(serverOutput));
                    serverOutput.flush();
                  }
                } catch (IOException e) {
                  // Not much to be done if serverOutput throws an exception
                }
              }
            } catch (JSONException ex2) {
              disposition = RequestState.ERROR;
              try {
                synchronized (serverOutput) {
                  serverOutput.write(ScionText.commandErrorMissing_message + PlatformUtil.NL);
                  ex2.printStackTrace(new PrintWriter(serverOutput));
                  serverOutput.flush();
                }
              } catch (IOException ex3) {
                // Not much to be done if serverOutput throws an exception
              }
            }

            try {
              synchronized (serverOutput) {
                serverOutput.write(ScionText.commandProcessingFailed_message + PlatformUtil.NL);
                ex.printStackTrace(new PrintWriter(serverOutput));
                serverOutput.flush();
              }
            } catch (IOException e) {
              // Not much to be done if serverOutput throws an exception
            }
          } catch (ClassCastException cce) {
            disposition = RequestState.ERROR;
            try {
              synchronized (serverOutput) {
                serverOutput.write(NLS.bind(ScionText.commandUnexpectedResult_message, result) + PlatformUtil.NL);
                cce.printStackTrace(new PrintWriter(serverOutput));
                serverOutput.flush();
              }
            } catch (IOException e) {
              // Not much to be done if serverOutput throws an exception
            }
          }
        }
      } catch (JSONException ex) {
        disposition = RequestState.ERROR;
        try {
          synchronized (serverOutput) {
            serverOutput.write(ScionText.errorReadingId_warning + PlatformUtil.NL);
            ex.printStackTrace(new PrintWriter(serverOutput));
            serverOutput.flush();
          }
        } catch (IOException ex2) {
          // Nothing we can really do about this.
        }
      } finally {
        if (command != null) {
          command.setResponse(null);
        }
        if (rState != null) {
          if (rState.synchronous) {
            rState.status = disposition;
            synchronized (rState) {
              rState.notifyAll();
            }
          }
        }
      }
    }
    
    return retval;
  }

  private boolean checkResponseVersion(JSONObject response) {
    boolean retval = true;
    try {
      String version = response.getString("version");
      if (!version.equals(PROTOCOL_VERSION)) {
        ScionPlugin.logWarning(NLS.bind(ScionText.commandVersionMismatch_warning, version, PROTOCOL_VERSION), null);
        retval = false;
      }
    } catch (JSONException ex) {
      ScionPlugin.logWarning(ScionText.errorReadingVersion_warning, ex);
    }
    
    return retval;
  }

  private int nextSequenceNumber() {
    return nextSequenceNumber.getAndIncrement();
  }

  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Internal classes
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
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
          String responseString = serverOutStream.readLine();
          if (responseString != null) {
            if (responseString.startsWith(PREFIX)) {
              response = new JSONObject(new JSONTokener(responseString.substring(PREFIX.length())));
              Trace.trace(FROM_SERVER_PREFIX, "%s", response.toString());
              processResponse(response);
            } else {
              String errMsg = NLS.bind(ScionText.scionServerInvalidResponsePrefix_message, PREFIX);
              synchronized (serverOutput) {
                serverOutput.write(errMsg + PlatformUtil.NL);
                serverOutput.flush();
              }
              Trace.trace(FROM_SERVER_PREFIX, errMsg);
            }
          } else {
            synchronized (serverOutput) {
              serverOutput.write(ScionText.scionServerGotEOF_message + PlatformUtil.NL);
            }
            Trace.trace(FROM_SERVER_PREFIX, ScionText.scionServerGotEOF_message);
            stopServer();
          }
        } catch (JSONException ex) {
          try {
            synchronized (serverOutput) {
              serverOutput.write(ScionText.scionJSONParseException_message + PlatformUtil.NL);
              serverOutput.flush();
            }
            ex.printStackTrace(new PrintWriter(serverOutput));
            stopServer();
          } catch (IOException ex2) {
            stopServer();
          }
        } catch (IOException ex) {
        }
        // long t1=System.currentTimeMillis();
        // System.err.println("receive+parse:"+(t1-t0));
      }
    }
  }

  /**
   * Command request dispatching state.
   */
  private class RequestState {
    /** The command being executed. This object's doProcessResult method will be invoked */
    final ScionCommand command;
    /** Command status: WAITING or DONE. */
    int status;
    /** Synchronous (true) vs. asynchronous (false) completion.
     */
    boolean synchronous;
    /** WAITING state: command is waiting for a response. */
    static final int WAITING = 0;
    /** DONE state: command's response has been received */
    static final int DONE = 1;
    /** ERROR state: An error was encountered in processRequest(). */
    static final int ERROR = 2;
    
    public RequestState(final ScionCommand command, boolean synchronous) {
      this.command = command;
      this.synchronous = synchronous;
      this.status = WAITING;
    }
  }
}
