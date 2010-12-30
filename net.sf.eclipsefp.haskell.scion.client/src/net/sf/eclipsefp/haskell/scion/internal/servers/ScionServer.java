package net.sf.eclipsefp.haskell.scion.internal.servers;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import net.sf.eclipsefp.haskell.scion.client.ScionEventType;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.QuitCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
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
  protected static final String              TO_SERVER_PREFIX     = "<< ";
  /** Message prefix for responses received from the server */
  protected static final String              FROM_SERVER_PREFIX   = ">> ";
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

  /** logs output **/
  protected OutputWriter                     outputWriter;
  /** Server interaction verbosity */
  protected boolean                          verboseInteraction;
  
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
    this.outputWriter = null;
    this.verboseInteraction = false;
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
    this.serverName = this.getClass().getSimpleName() + "/" + ScionText.noproject;
    this.serverExecutable = null;
    this.serverOutput = null;
    this.directory = null;

    this.process = null;
    this.serverOutStream = null;
    this.serverInStream = null;
    this.nextSequenceNumber = new AtomicInteger(1);
    this.commandQueue = new HashMap<Integer, ScionCommand>();
    this.outputWriter = null;
    this.verboseInteraction = false;
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
    Trace.trace(serverName, "Starting server.");

    cqMonitor = new CommandQueueMonitor(serverName + "-CommandQueueMonitor");
    cqMonitor.start();

    outputWriter = new OutputWriter(serverName + "-Writer");
    outputWriter.start();

    doStartServer(project);

    Trace.trace(serverName, "Server started.");
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
   * @param cleanly If true, send a quit command to the server, otherwise, apply brutal force and kill the server.
   * 
   * @note This method should not be overridden by subclasses. Subclasses should
   *       override {@link doStopServer doStopServer} instead. This is a
   *       protocol design method, where {@link ScionServer ScionServer}
   *       implements code that must be executed before or after the subclass'
   *       server launch.
   */
  public final void stopServer(boolean cleanly) {
    Trace.trace(serverName, "Stopping server");

    try {
      // If there are pending commands for which sendCommand() is waiting, 
      // make sure that they get awakened:
      for (ScionCommand cmd : commandQueue.values() ) {
        synchronized (cmd) {
          cmd.setCommandError();
          cmd.notifyAll(); 
        }
      }
      
      // Clear out the queue.
      commandQueue.clear();
      
      if (cleanly) {
        // Send the quit command, but don't bother waiting for the result to arrive. If we do wait for the
        // result, we could potentially wait forever.
        internalSendCommand(new QuitCommand());
      }
      
      // Let the subclass do its thing. BEFORE we close the streams, to give
      // the sub class a chance to close its own things properly
      doStopServer();

      if (outputWriter != null) {
        outputWriter.setTerminate();
        outputWriter.interrupt();
        outputWriter.join();
        outputWriter = null;
      }

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
      if (cqMonitor != null) {
        cqMonitor.setTerminate();
        cqMonitor.interrupt();
        cqMonitor.join();
        cqMonitor = null;
      }
    } catch (Throwable ex) {
      // ignore
    } finally {
      // Ensure the command queue is really drained
      commandQueue.clear();
    }

    Trace.trace(serverName, "Server stopped");
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
    
    if (serverInStream == null ) {
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
      outputWriter.addMessage(getClass().getSimpleName() + ".sendCommand encountered an exception:");
      outputWriter.addMessage(ex);
    } finally {
      if (verboseInteraction) {
        outputWriter.addMessage(TO_SERVER_PREFIX + command.toJSONString());
      }
    }
    
    return retval;
  }

  /**
   * Dequeue the command as indexed by its identifier, sets the command's
   * response member 
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
    outputWriter.addMessage(message);
    ScionPlugin.logError(message, exc);
  }

  /**
   * Notify the {@link ScionInstance}'s listeners that an abnormal termination
   * just happened
   */
  protected void signalAbnormalTermination() {
    if (project != null) {
      ScionInstance scionInstance = ScionPlugin.getScionInstance(project);
      Assert.isNotNull(scionInstance);
      scionInstance.stop(false);
      scionInstance.notifyListeners(ScionEventType.ABNORMAL_TERMINATION);
    } else {
      // The shared scion-server instance just abnormally terminated.
      ScionPlugin.getSharedScionInstance().notifyListeners(ScionEventType.ABNORMAL_TERMINATION);
    }
  }
  
  /**
   * Set verbose interaction flag
   */
  public void setVerboseInteraction(final boolean flag) {
    verboseInteraction = flag;
  }
  
  /**
   * Get the verbose interaction flag
   */
  public boolean getVerboseInteraction() {
    return verboseInteraction;
  }
  
  /**
   * Command queue monitor: Report when we think we have a stalled queue.
   */
  public class CommandQueueMonitor extends Thread {
    private final static int TMO = (350 * 1000) / 100; // 3.50 * 1000 milliseconds
    private final static int MAXSTALLS = 10;           // 35 seconds after initial stall
    private boolean terminateFlag;
    private int lastDepth;
    private int stallCount;
    private Set<Entry<Integer, ScionCommand>> lastQueue;
    
    public CommandQueueMonitor(final String threadName) {
      super();
      terminateFlag = false;
      lastDepth = -1;
      stallCount = 0;
      lastQueue = null;
      
      setName(threadName);
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
            lastQueue = null;
          } else if (lastDepth > 0) {
            // Maybe we're stalled?
            Set<Entry<Integer, ScionCommand>> commands = null;
            
            synchronized (commandQueue) {
              commands = commandQueue.entrySet();
            }
            
            ++stallCount;

            if ( lastQueue != null && commands.equals(lastQueue) ) {
              if (stallCount > MAXSTALLS) {
                StringBuffer diagMsg = new StringBuffer();
              
                diagMsg.append("Possibly stalled queue [").append(serverName).append("], depth = ");
                diagMsg.append(lastDepth).append(PlatformUtil.NL);
                diagMsg.append("Waiting commands:").append(PlatformUtil.NL);
              
                for (Entry<Integer, ScionCommand> cmd : commands) {
                  diagMsg.append("  [").append(cmd.getKey()).append("] ");
                  diagMsg.append(cmd.getValue().getMethod()).append(PlatformUtil.NL);
                }
              
                ScionPlugin.logInfo(diagMsg.toString());
              }
            } else {
              // Only re-assign as needed
              lastQueue = commands;
            }
          }
        } catch (InterruptedException irq) {
          // Ignore it.
        }
      }
    }
  }
  
  /**
   * A separate thread to write the communication with the server see
   * https://bugs.eclipse.org/bugs/show_bug.cgi?id=259107
   */
  public class OutputWriter extends Thread {
    /** the message list **/
    private final LinkedList<String> messages   = new LinkedList<String>();
    /** should we stop? **/
    private boolean                  terminateFlag;

    public OutputWriter(String name) {
      super(name);
    }

    public void setTerminate() {
      terminateFlag = true;
    }

    public void addMessage(String msg) {
      synchronized (messages) {
        messages.add(msg);
        messages.notify();
      }
    }

    public void addMessage(char[] buf, int start, int length) {
      synchronized (messages) {
        messages.add(new String(buf, start, length));
        messages.notify();
      }
    }

    public void addMessage(Exception e) {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);
      e.printStackTrace(pw);
      pw.flush();
      synchronized (messages) {
        messages.add(sw.toString());
        messages.notify();
      }
    }

    @Override
    public void run() {
      while (!terminateFlag && serverOutput != null) {
        String m = null;
        synchronized (messages) {
          try {
            while (messages.isEmpty()) {
              messages.wait();
            }

          } catch (InterruptedException ignore) {
            // noop
          }
          if (!messages.isEmpty()) {
            m = messages.removeFirst();
          }
        }
        if (m != null) {
          Trace.trace(serverName, m);
          try {
            final int mLen = m.length();
            int i = 0;

            // Break the message up into 1K chunks for better UI responsiveness, since this all is going to
            // an IOConsole.
            while ( mLen - i > 1024 ) {
              serverOutput.write(m, i, 1024);
              serverOutput.flush();
              i += 1024;
            }
            serverOutput.write(m, i, mLen - i);
            serverOutput.write(PlatformUtil.NL);
            serverOutput.flush();
          } catch (IOException ex) {
            if (!terminateFlag) {
              ScionPlugin.logError(ScionText.scionServerOutputError_message, ex);
            }
          } catch (SWTException se) {
            // probably device has been disposed
            if (!terminateFlag) {
              ScionPlugin.logError(ScionText.scionServerOutputError_message, se);
            }
          }
        }
      }
    }
  }
}
