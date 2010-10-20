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
import org.eclipse.swt.SWTException;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * some helper code to implement a ScionServer
 * 
 * @author JP Moresmau
 * 
 */
public abstract class ScionServer {
  protected static final String CLASS_PREFIX         = "[ScionServer]";
  protected static final String SERVER_STDOUT_PREFIX = "[scion-server]";
  /** Message prefix for commands send to the server */
  protected static final String      TO_SERVER_PREFIX   = "[scion-server] << ";
  /** Message prefix for responses received from the server */
  protected static final String      FROM_SERVER_PREFIX = "[scion-server] >> ";
  /** The Scion protocol version number */
  private static final String      PROTOCOL_VERSION = "0.1";
  /**
   * Path to the server executable that is started and with whom EclipseFP
   * communicates
   */
  protected IPath               serverExecutable;
  /**
   * Project name, used to disambiguate server from each other in the tracing output.
   */
  protected final String        projectName;
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
  /** Request identifier */
  private final AtomicInteger   nextSequenceNumber;
  
  /** Command queue, to deal with both synchronous and asynchronous commands */
  protected final Map<Integer, ScionCommand> commandQueue;

  /**
   * The constructor
   * @param projectName TODO
   * @param serverExecutable
   *          The scion-server executable
   * @param serverOutput
   *          The scion-server's logging and trace stream.
   * @param directory
   *          The scion-server's working directory
   */
  public ScionServer(String projectName, IPath serverExecutable, Writer serverOutput, File directory) {
    this.projectName = projectName;
    this.serverExecutable = serverExecutable;
    this.serverOutput = serverOutput;
    this.directory = directory;
    
    this.process = null;
    this.serverOutStream = null;
    this.serverInStream = null;
    this.nextSequenceNumber = new AtomicInteger(1);
    this.commandQueue =  new HashMap<Integer, ScionCommand>();
  }
  
  /** The default constructor. This is only used by NullScionServer 
   * @param projectName TODO*/
  protected ScionServer() {
    this.projectName = ScionText.noproject;
    this.serverExecutable = null;
    this.serverOutput = null;
    this.directory = null;
    
    this.process = null;
    this.serverOutStream = null;
    this.serverInStream = null;
    this.nextSequenceNumber = new AtomicInteger(1);
    this.commandQueue =  new HashMap<Integer, ScionCommand>();
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
   * {@link ScionServer ScionServer} implements code that must be executed
   * before or after the subclass' server launch.
   */
  public final void startServer() throws ScionServerStartupException {
    Trace.trace(CLASS_PREFIX, "Starting server " + getClass().getSimpleName() + ":" + projectName);
    doStartServer(projectName);
    Trace.trace(CLASS_PREFIX, "Server started for " + getClass().getSimpleName() + ":" + projectName);
 }

  /** Subclass' hook for starting up their respective server processes.
   * 
   * @param projectName The project name
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
   * {@link ScionServer ScionServer} implements code that must be executed
   * before or after the subclass' server launch.
   */
  public final void stopServer() {
    Trace.trace(CLASS_PREFIX, "Stopping server");

    try {
       // Let the subclass do its thing.
       // BEFORE we close the streams, to give the sub class a chance to close its own things properly
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
      // Keep track of this request in the command queue
      int seqNo = nextSequenceNumber.getAndIncrement();
      command.setSequenceNumber(seqNo);
      
      synchronized ( commandQueue ) {
        commandQueue.put(new Integer(seqNo), command);
      }
      
      String jsonString = command.toJSONString();

      try {
        serverInStream.write(command.toJSONString() + PlatformUtil.NL);
        serverInStream.flush();
      } catch (IOException ex) {
        try {
          serverOutput.write(getClass().getSimpleName() + ".sendCommand encountered an exception:" + PlatformUtil.NL);
          ex.printStackTrace(new PrintWriter(serverOutput));
          serverOutput.flush();
        } catch (IOException e) {
          stopServer();
        }
      } finally {
        final String toServer = projectName + "/" + TO_SERVER_PREFIX;
        Trace.trace(toServer, "%s", jsonString);
        if (Trace.isTracing()) {
          try {
            serverOutput.write(TO_SERVER_PREFIX + jsonString + PlatformUtil.NL);
            serverOutput.flush();
          } catch (IOException ex) {
            // Ignore this (something creative here?) 
          } catch (SWTException se){
        	  // device is disposed when shutting down, we hope
          }
        }
      }
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
      command.setIsSync();
      
      synchronized (command) {
        sendCommand(command);
        while (command.isWaiting()) {
          try {
            command.wait();
          } catch (InterruptedException ex) {
            // We'll spin until the request is processed...
          }
        }
      }
      retval = command.isDone();
    }
    return retval;
  }
  
  /**
   * Check the server's protocol version. This just generates a warning if the
   * version numbers do not match.
   */
  public void checkProtocol() {
    sendCommand(new ConnectionInfoCommand());
  }
  
  /**
   * Parses the given response string and stores the command result in this
   * object.
   */
  public boolean processResponse(JSONObject response) {
    boolean retval = false;
    
    if (checkResponseVersion(response)) {
      int id = 0;
      ScionCommand command = null;
      
      try {
        id = response.getInt("id");
        
        synchronized (commandQueue) {
          Integer key = new Integer(id);
          command = commandQueue.remove(key);
        }
        
        Object result = response.get("result");
        try {
          command.setResponse(response);
          command.processResult(result);
          command.setCommandDone();
          command.runSuccessors(ScionServer.this);
          retval = true;
        } catch (JSONException ex) {
          try {
            JSONObject error = response.getJSONObject("error");
            String name = error.getString("name");
            String message = error.getString("message");
            if (!command.onError(ex, name, message)) {
              command.setCommandError();
              try {
                final String errMsg = NLS.bind(ScionText.commandError_message, name, message);
                
                serverOutput.write(errMsg + PlatformUtil.NL);
                ex.printStackTrace(new PrintWriter(serverOutput));
                serverOutput.flush();
                
                ScionPlugin.logError(errMsg, ex);
              } catch (IOException e) {
                // Not much to be done if serverOutput throws an exception
              }
            }
          } catch (JSONException ex2) {
            command.setCommandError();
            try {
              serverOutput.write(ScionText.commandErrorMissing_message + PlatformUtil.NL);
              ex2.printStackTrace(new PrintWriter(serverOutput));
              serverOutput.flush();
            } catch (IOException ex3) {
              // Not much to be done if serverOutput throws an exception
            }
          }

          try {
            serverOutput.write(ScionText.commandProcessingFailed_message + PlatformUtil.NL);
            ex.printStackTrace(new PrintWriter(serverOutput));
            serverOutput.flush();
          } catch (IOException e) {
            // Not much to be done if serverOutput throws an exception
          }
        } catch (ClassCastException cce) {
          command.setCommandError();
          try {
            final String errMsg = NLS.bind(ScionText.commandUnexpectedResult_message, result);
            serverOutput.write(errMsg.concat(PlatformUtil.NL));
            cce.printStackTrace(new PrintWriter(serverOutput));
            serverOutput.flush();
            
            ScionPlugin.logError(errMsg, cce);
          } catch (IOException e) {
            // Not much to be done if serverOutput throws an exception
          }
        } finally {
          command.setResponse(null);
        }
      } catch (JSONException ex) {
        command.setCommandError();
        try {
          serverOutput.write(ScionText.errorReadingId_warning + PlatformUtil.NL);
          ex.printStackTrace(new PrintWriter(serverOutput));
          serverOutput.flush();
        } catch (IOException ex2) {
          // Nothing we can really do about this.
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
}
