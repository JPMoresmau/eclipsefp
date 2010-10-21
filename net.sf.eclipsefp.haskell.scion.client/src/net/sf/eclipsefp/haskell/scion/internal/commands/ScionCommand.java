package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.ICommandContinuation;
import net.sf.eclipsefp.haskell.scion.internal.servers.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;

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
  /** Commands to be executed after this command */
  private final List<ScionCommand>         successors;
  /** Methods to be run after the command has finished executing. */
  private final List<ICommandContinuation> continuations;
  /**
   * Used only for error reporting during response processing.
   */
  private JSONObject                       response;
  /** Command status: WAITING or DONE. */
  int                                      status;
  /**
   * Synchronous (true) vs. asynchronous (false) completion.
   */
  boolean                                  isSync;
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
    status = WAITING;
    isSync = false;
    successors = new LinkedList<ScionCommand>();
    continuations = new LinkedList<ICommandContinuation>();
  }

  /** Set the continuation that needs to be run when the job's status changes */
  public void addContinuation(final ICommandContinuation continuation) {
    continuations.add(continuation);
  }

  /** Set the sequence number of this command */
  public void setSequenceNumber(int sequenceNumber) {
    this.sequence = sequenceNumber;
  }

  /** Set the response JSON object */
  public void setResponse(JSONObject response) {
    this.response = response;
  }

  /** Set the command's status to DONE */
  public void setCommandDone() {
    setCommandStatus(DONE);
  }

  /** Set the command's status to ERROR */
  public void setCommandError() {
    setCommandStatus(ERROR);
  }

  /**
   * Internal method to set the command's status and signal waiting objects that
   * the status has changed.
   */
  private void setCommandStatus(final int newStatus) {
    status = newStatus;
    if (isSync) {
      synchronized (this) {
        notifyAll();
      }
    }
  }

  /** Predicate to test if command is still waiting for a response. */
  public boolean isWaiting() {
    return (status == WAITING);
  }

  /** Predicate to test if the command is in the DONE state. */
  public boolean isDone() {
    return (status == DONE);
  }

  /** Set synchronous command flag */
  public void setIsSync() {
    isSync = true;
  }

  /** Predicate to test if the command has been launch synchronously.  */
  public boolean isSync() {
	return isSync;
  }
  
  /**
   * Run successor commands queued in the {@link #successors} list.
   * 
   * @return True if this command is synchronously sent and all of the
   *         successors also succeed. For asynchronous commands, always returns
   *         true.
   */
  public boolean runSuccessors(ScionServer server) {
    boolean retval = true;
    if (isSync) {
      for (ScionCommand sc : successors) {
        retval &= server.sendCommandSync(sc);
      }
    } else {
      for (ScionCommand sc : successors)
        server.sendCommand(sc);
    }

    return retval;
  }

  public boolean onError(String name, String message) {
    return false;
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
  protected abstract String getMethod();

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

  public final void processResult(Object result) throws JSONException {
    doProcessResult(result);
    for (ICommandContinuation continuation : continuations) {
      continuation.commandContinuation();
    }
  }

  protected void doProcessResult(Object result) throws JSONException {
    // Base class does nothing.
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
        info += "\n" + NLS.bind(ScionText.scionFailedResponse_message, prettyPrint(response));
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

  /**
   * Add a successor to this command that is executed once this command
   * completes successfully.
   * 
   * @param successor
   *          The successor command.
   */
  public void addSuccessor(ScionCommand successor) {
    successors.add(successor);
  }
}
