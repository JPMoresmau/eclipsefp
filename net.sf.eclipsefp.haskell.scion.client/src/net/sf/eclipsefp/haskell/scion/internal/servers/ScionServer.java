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

import net.sf.eclipsefp.haskell.scion.client.ScionEventType;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWTException;
import org.json.JSONObject;

/**
 * some helper code to implement a ScionServer
 * 
 * @author JP Moresmau
 * 
 */
public abstract class ScionServer {
  protected static final String              CLASS_PREFIX         = "[ScionServer]";
  protected static final String              SERVER_STDOUT_PREFIX = "[scion-server]";
  /** Message prefix for commands send to the server */
  protected static final String              TO_SERVER_PREFIX     = "[scion-server] << ";
  /** Message prefix for responses received from the server */
  protected static final String              FROM_SERVER_PREFIX   = "[scion-server] >> ";
  /** The Scion protocol version number */
  public static final String                 PROTOCOL_VERSION     = "0.1";
  /** The Scion "wire protocol" version number reported by the connection-info command */
  public static final int                    WIRE_PROTOCOL_VERSION = 1;
  /**
   * Path to the server executable that is started and with whom EclipseFP
   * communicates
   */
  protected IPath                            serverExecutable;
  /**
   * The project with which this server is associated.
   */
  protected final IProject                   project;
  /**
   * The server's name, used for logging
   */
  protected final String                     serverName;
  /** Server logging output stream, generally tends to be a Eclipse console */
  protected Writer                           serverOutput;
  /** Working directory where the server operates, can be null if no project. */
  protected File                             directory;
  /** The scion-server process */
  protected Process                          process;
  /** scion-server's output stream, read by EclipseFP */
  protected BufferedReader                   serverOutStream;
  /** scion-server's input stream, written to by EclipseFP */
  protected BufferedWriter                   serverInStream;
  /** Request identifier */
  private final AtomicInteger                nextSequenceNumber;

  /** Command queue, to deal with both synchronous and asynchronous commands */
  protected final Map<Integer, ScionCommand> commandQueue;
  /** Command queue monitor, track potential stalls */
  protected CommandQueueMonitor              cqMonitor;

  /**
   * The constructor
   * 
   * @param project
   *          TODO
   * @param serverExecutable
   *          The scion-server executable
   * @param serverOutput
   *          The scion-server's logging and trace stream.
   * @param directory
   *          The scion-server's working directory
   */
  public ScionServer(IProject project, IPath serverExecutable, Writer serverOutput, File directory) {
    this.project = project;
    this.serverName = getClass().getSimpleName() + "/" + (project != null ? project.getName() : ScionText.noproject);
    this.serverExecutable = serverExecutable;
    this.serverOutput = serverOutput;
    this.directory = directory;

    this.process = null;
    this.serverOutStream = null;
    this.serverInStream = null;
    this.nextSequenceNumber = new AtomicInteger(1);
    this.commandQueue = new HashMap<Integer, ScionCommand>();
    this.cqMonitor = null;
  }

  /**
   * The default constructor. This is only used by NullScionServer
   * 
   * @param projectName
   *          TODO
   */
  protected ScionServer() {
    this.project = null;
    this.serverName = ScionText.noproject;
    this.serverExecutable = null;
    this.serverOutput = null;
    this.directory = null;

    this.process = null;
    this.serverOutStream = null;
    this.serverInStream = null;
    this.nextSequenceNumber = new AtomicInteger(1);
    this.commandQueue = new HashMap<Integer, ScionCommand>();
    this.cqMonitor = null;
  }

  /** Redirect the logging stream */
  public void setOutputStream(final Writer outStream) {
    serverOutput = outStream;
  }

  /**
   * Start the server process.
   * 
   * @note This method should not be overridden by subclasses. Subclasses should
   *       override {@link doStartServer doStartServer} instead. This is a
   *       protocol design method, where {@link ScionServer ScionServer}
   *       implements code that must be executed before or after the subclass'
   *       server launch.
   */
  public final void startServer() throws ScionServerStartupException {
    final String projectName = (project != null ? project.getName() : ScionText.noproject);
    Trace.trace(CLASS_PREFIX, "Starting server " + getClass().getSimpleName() + ":" + projectName);
    doStartServer(project);
    cqMonitor = new CommandQueueMonitor();
    cqMonitor.start();
    Trace.trace(CLASS_PREFIX, "Server started for " + getClass().getSimpleName() + ":" + projectName);
  }

  /**
   * Subclass' hook for starting up their respective server processes.
   * 
   * @param project
   *          The project name
   */
  protected void doStartServer(IProject project) throws ScionServerStartupException {
    // Does nothing...
    serverOutStream = null;
    serverInStream = null;
  }

  /**
   * Stop the server process.
   * 
   * @note This method should not be overridden by subclasses. Subclasses should
   *       override {@link doStopServer doStopServer} instead. This is a
   *       protocol design method, where {@link ScionServer ScionServer}
   *       implements code that must be executed before or after the subclass'
   *       server launch.
   */
  public final void stopServer() {
    Trace.trace(CLASS_PREFIX, "Stopping server");

    try {
       // Let the subclass do its thing. BEFORE we close the streams, to give
       // the sub class a chance to close its own things properly
       doStopServer();
        
      if (serverOutStream != null) {
        serverOutStream.close();
        serverOutStream = null;
      }
      if (serverInStream != null) {
        serverInStream.close();
        serverInStream = null;
      }
      

      // Then kill off the server process.
      if (process != null) {
        process.destroy();
        process = null;
      }
      
      // And the command queue monitor
      if ( cqMonitor != null ) {
        cqMonitor.setTerminate();
        cqMonitor.interrupt();
        cqMonitor.join();
        cqMonitor = null;
      }
      
      commandQueue.clear();
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
   * Send a command to the server, wait for a response and process the result. This
   * is the synchronous command interface.
   */
  public boolean sendCommand(ScionCommand command) {
    boolean retval = false;
    
    if (serverInStream == null) {
      return retval;
    }

    if ( internalSendCommand(command) ) {
      // Wait for receiver to get the response or transition out of its WAITING status (usually the ERROR state)
      synchronized (command) {
        while ( command.isWaiting() && !command.hasResponse() ) {
          try {
            command.wait();
          } catch (InterruptedException ex) {
            // We'll spin until the request is processed...
          }
        }
      }

      // If an error occurred, such as a JSON exception, don't process the result.
      if ( !command.isError() )
        retval = command.processResult(this);
    }
    
    return retval;
  }

  /**
   * Queue command, send it to the server. This is the asynchronous interface.
   */
  public boolean queueCommand(ScionCommand command) {
    if (serverInStream != null) {
      assert (!command.hasContinuations());
      command.asyncResponseProcessor(this, serverName);
      return internalSendCommand(command);
    }
    
    return true;
  }

  /**
   * The internal method that puts a command on the commandQueue and sends the JSON-ified command string
   * to scion-server. This is the common code for sendCommand() and queueCommand().
   * 
   * @param command
   * @return
   */
  private boolean internalSendCommand(ScionCommand command) { 
    boolean retval = false;
    // Keep track of this request in the command queue
    int seqNo = nextSequenceNumber.getAndIncrement();

    synchronized (commandQueue) {
      command.setSequenceNumber(seqNo);
      commandQueue.put(new Integer(seqNo), command);
    }

    try {
      serverInStream.write(command.toJSONString() + PlatformUtil.NL);
      serverInStream.flush();
      retval = true;
    } catch (IOException ex) {
      try {
        serverOutput.write(getClass().getSimpleName() + ".sendCommand encountered an exception:" + PlatformUtil.NL);
        ex.printStackTrace(new PrintWriter(serverOutput));
        serverOutput.flush();
      } catch (IOException e) {
        stopServer();
      }
    } finally {
      String jsonString = command.toJSONString();

      Trace.trace(serverName, TO_SERVER_PREFIX.concat(jsonString));
      if (Trace.isTracing()) {
        try {
          serverOutput.write(TO_SERVER_PREFIX + jsonString + PlatformUtil.NL);
          serverOutput.flush();
        } catch (IOException ex) {
          // Ignore this (something creative here?)
        } catch (SWTException se) {
          // device is disposed when shutting down, we hope
        }
      }
    }
    
    return retval;
  }

  /**
   * Parses the given response string and stores the command result in this
   * object.
   */
  public boolean processResponse(JSONObject response) {
    int id = response.optInt("id", -1);
    ScionCommand command = null;

    if (id <= 0) {
      // Command identifiers are always greater than 0.
      logMessage(ScionText.errorReadingId_warning, null);
      return false;
    }
    
    // Ensure command is always dequeued
    synchronized (commandQueue) {
      Integer key = new Integer(id);
      command = commandQueue.remove(key);
    }
    
    if (command == null) {
      // Should have found the command in the queue...
      logMessage(NLS.bind(ScionText.commandIdMismatch_warning, id), null);
      return false;
    }
    
    return command.setResponse(response, this);
  }

  /**
   * Log a message to both the scion-server's output stream and the Eclipse log
   * file.
   * 
   * @param message
   *          The error message to log
   * @param exc
   *          The exception associated with the message, may be null.
   */
  public void logMessage(final String message, final Throwable exc) {
    try {
      serverOutput.write(message + PlatformUtil.NL);
      serverOutput.flush();
    } catch (IOException ioex) {
      // Not much we can do.
    }

    ScionPlugin.logError(message, exc);
  }

  /**
   * Notify the {@link ScionInstance}'s listeners that an abnormal termination
   * just happened
   */
  protected void signalAbnormalTermination() {
    ScionInstance scionInstance = ScionPlugin.getScionInstance(project);
    assert (scionInstance != null);
    if (scionInstance != null) {
      scionInstance.stop();
      scionInstance.notifyListeners(ScionEventType.ABNORMAL_TERMINATION);
    }
  }
  
  /**
   * Command queue monitor: Report when we think we have a stalled queue.
   */
  public class CommandQueueMonitor extends Thread {
    private final static int TMO = (175 * 1000) / 100; // 1.75 * 1000
    private final static int MAXSTALLS = 5;
    private boolean terminateFlag;
    private int lastDepth;
    private int stallCount;
    
    public CommandQueueMonitor() {
      super();
      terminateFlag = false;
      lastDepth = -1;
      stallCount = 0;
    }
    
    public void setTerminate() {
      terminateFlag = true;
    }
    
    public void run() {
      while ( !terminateFlag ) {
        try {
          Thread.sleep(TMO);
          
          int depth = commandQueue.size();
          if (depth != lastDepth) {
            lastDepth = depth;
            stallCount = 0;
          } else if (lastDepth > 0) {
            // Maybe we're stalled?
            ++stallCount;
            if (stallCount > MAXSTALLS) {
              ScionPlugin.logInfo(String.format("Possibly stalled queue [%s], depth = %d", serverName, lastDepth ) );
            }
          }
        } catch (InterruptedException irq) {
          // Ignore it.
        }
      }
    }
  }
}
