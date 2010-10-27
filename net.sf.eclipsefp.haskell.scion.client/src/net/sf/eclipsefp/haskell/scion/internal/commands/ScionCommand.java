package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import net.sf.eclipsefp.haskell.scion.internal.servers.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.osgi.util.NLS;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A command that can be sent to the Scion server. After being run, it can be
 * queried for the response.
 * 
 * @author Thomas ten Cate
 */
public abstract class ScionCommand {
  /** Request sequence number, set externally by an AbstractServer object. */
  private int                              sequence;
  /** The Job that processes the response */
  private Job                              processResultJob;
  /** The server name associated with processResultJob (used solely for setting the Job's name to something
   * readable. */
  private String                           serverName;
  /** Commands to be executed after this command */
  private final List<ScionCommand>         successors;
  /** Jobs to be run after the command has finished executing. */
  private final List<Job>                  continuations;
  /**
   * The general JSON response from the server, may be a {@link JSONArray} or {@link JSONObject}.
   */
  protected Object                         response;
  /** Command status: WAITING or DONE. */
  int                                      status;
  /** WAITING state: command is waiting for a response. */
  static final int                         WAITING = 0;
  /** DONE state: command's response has been received */
  static final int                         DONE    = 1;
  /** ERROR state: An error was encountered in processRequest(). */
  static final int                         ERROR   = 2;

  /**
   * Constructs a new scion-server command.
   */
  public ScionCommand() {
    processResultJob = null;
    serverName = "[No server]";
    successors = new ArrayList<ScionCommand>();
    continuations = new ArrayList<Job>();
    status = WAITING;
    response = null;
  }

  /** Arrange for asynchronous response processing... */
  public void asyncResponseProcessor(final ScionServer server, final String serverName) {
    // The Job that gets invoked when a response is received, setResponse() will schedule
    // this Job at the appropriate time.
    this.serverName = serverName;
    processResultJob = new Job(NLS.bind(ScionText.process_result_job, getMethod(), serverName)) {
      @Override
      protected IStatus run(IProgressMonitor monitor) {
        processResult(server);
        return Status.OK_STATUS;
      }
    };
  }
  
  /** Set the sequence number of this command */
  public void setSequenceNumber(int sequenceNumber) {
    sequence = sequenceNumber;
    if (processResultJob != null) {
      processResultJob.setName(NLS.bind(ScionText.process_result_job_arg,
                                        new Object[] { getMethod(), serverName, sequenceNumber }) );
    }
  }

  /** Set the response JSON object, fire off the result processing job.
   * 
   * @param result The JSON result object from scion-server
   * @param server The server, in case we need to process an asynchronous command's successors
   * @throws JSONException
   */
  public boolean setResponse(JSONObject result, ScionServer server) {
    boolean retval = false;
    
    if (!checkResponseVersion(result)) {
      setCommandError();
    } else {
      Object jsonResult = result.opt("result");
      if (jsonResult != null) {
        response = jsonResult;
        retval = true;
      } else {
        JSONObject error = result.optJSONObject("error");
        if (error != null) {
          try {
            String name = error.getString("name");
            String message = error.getString("message");
            
            if (!onError(name, message)) {
              server.logMessage(NLS.bind(ScionText.commandError_message, name, message), null);
            }
            
            // Prevent processReulst() from running, since response will be null.
            setCommandError();
          } catch (JSONException ex2) {
            setCommandError();
            server.logMessage(ScionText.commandProcessingFailed_message, ex2);
          }
        } else {
          setCommandError();
          server.logMessage(NLS.bind(ScionText.commandErrorMissing_message, result), null);
        }
      }
    }

    // If this is an asynchronous command, processResultJob will be non-null.
    if (processResultJob == null) {
      // Synchronous command path: someone is wait()-ing to be notified....
      synchronized (this) {
        notifyAll();
      }
    } else {
      if (retval) {
        // Schedule the result processing for later, but only if we have an attached response.
        processResultJob.schedule();
      }
    }

    return retval;
  }

  /**
   * Check the response from scion-server, ensure that the version number
   * matches. Otherwise, yell at the user.
   */
  private boolean checkResponseVersion(JSONObject response) {
    String version = response.optString("version");
    return (version != null && version.equals(ScionServer.PROTOCOL_VERSION));
  }

  /** Set the command's status to DONE */
  public void setCommandDone() {
    status = DONE;
  }

  /** Set the command's status to ERROR */
  public void setCommandError() {
    response = null;
    status = ERROR;
  }

  /** Predicate to test if command is still waiting for a response. */
  public boolean isWaiting() {
    return (status == WAITING);
  }

  /** Predicate to test if the command is in the DONE state. */
  public boolean isDone() {
    return (status == DONE);
  }
  
  /** Predicate to test if the command is in the ERROR state. */
  public boolean isError() {
    return (status == ERROR);
  }

  /** Predicate to test if the command has received a response */
  public boolean hasResponse() {
    return (response != null);
  }

  /** Base class case for onError handling */
  public boolean onError(String name, String message) {
    return false;
  }
  
  /** Set the continuation that needs to be run when the job's status changes */
  public void addContinuation( final Job continuation ) {
    continuations.add(continuation);
  }
  
  /**
   * Predicate that tests if the ScionCommand has continuations -- a command must have continuations
   * if the command is sent via {@link ScionServer#queueCommand(ScionCommand)}
   */
  public boolean hasContinuations() {
    return (continuations.size() > 0);
  }
  
  // /////////////
  // JSON stuff

  /**
   * Serializes the command to its JSON equivalent, ready to be sent to the
   * server.
   * 
   * @return a valid JSON string
   */
  public String toJSONString() {
    return toJSON().toString();
  }

  public JSONObject toJSON() {
    JSONObject json = new JSONObject();
    try {
      json.put("method", getMethod());
      json.put("params", getParams());
      json.put("id", sequence);
    } catch (JSONException e) {
      // should not happen
    }
    return json;
  }

  /**
   * Returns a human-readable representation of this command.
   * 
   * Currently returns a pretty-printed version of the JSON serialization.
   */
  public String toString() {
    return prettyPrint(toJSON());
  }

  /**
   * Returns the name of the "method" to be called on the server side, e.g.
   * "connection-info".
   */
  public abstract String getMethod();

  /**
   * Creates the params JSON object to be sent along with the command. The
   * default implementation returns an empty map; most subclasses will want to
   * override this.
   * 
   * @return a non-null JSONObject().
   */
  protected JSONObject getParams() throws JSONException {
    return new JSONObject();
  }

  /**
   * Process the received JSON result.
   * 
   * @param server The scion-server with whom the successor commands will communicate.
   * 
   * @return true if the result was processed successfully, all of the continuations have run and the
   * successors have been processed.
   */
  public final boolean processResult(final ScionServer server) {
    boolean retval = false;
    
    assert (response != null);
    // Process the response
    try {
      doProcessResult();
      setCommandDone();
      response = null;
      
      if (continuations.size() > 0) {
        ListIterator<Job> contIter = continuations.listIterator();
        while ( contIter.hasNext() ) {
          Job continuation = contIter.next();
          
          if ( !contIter.hasNext() ) {
            // Ensure that the last continuation runs the successors
            continuation.addJobChangeListener(new JobChangeAdapter() {
              @Override
              public void done(IJobChangeEvent ev) {
                if (ev.getResult().isOK()) {
                  ScionCommand.this.runSuccessors(server);
                }
              }
            } );
          }
          
          continuation.schedule();
        }
      } else {
        runSuccessors(server);
      }
      
      retval = true;
    } catch (JSONException jsonEx) {
      setCommandError();
      server.logMessage(ScionText.commandProcessingFailed_message, jsonEx);
    }
    
    return retval;
  }

  protected void doProcessResult() throws JSONException {
    // Base class does nothing.
  }
  
  /** Add a successor to this command that is executed once this command completes
   * successfully.
   *
   * @param successor The successor command.
   */
  public void addSuccessor(ScionCommand successor) {
    successors.add(successor);
  }
  
  /** Run successor commands queued in the {@link #successors} list. */
  public boolean runSuccessors(ScionServer server) {
    boolean retval = true;
    Iterator<ScionCommand> succIter = successors.iterator();
    while (retval && succIter.hasNext()) {
      ScionCommand sc = succIter.next();

      if ( sc.hasContinuations() ) {
        retval &= server.queueCommand(sc);
      } else {
        server.sendCommand(sc);
      }
    }
    
    return retval;
  }
  /**
   * Returns information about this command for use in error messages.
   * Guaranteed never to throw exceptions, so safe for use in exception
   * handling.
   * 
   * @return information about this command; never <code>null</code>
   */
  public String getErrorInfo() {
    String info = new String();
    try {
      info = NLS.bind(ScionText.scionFailedCommand_message, toString());
      if (this.response != null) {
        info += "\n" + NLS.bind(ScionText.scionFailedResponse_message, prettyPrint((JSONObject) response));
      }
    } catch (Throwable ex) {
      info = new String();
    }
    return info;
  }

  /** Pretty print a JSON object */
  protected static String prettyPrint(JSONObject json) {
    try {
      return json.toString(2);
    } catch (JSONException e) {
      // strangely, the not pretty-printed version does not throw at all
      return json.toString();
    }
  }
}
