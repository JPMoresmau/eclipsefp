package net.sf.eclipsefp.haskell.scion.internal.commands;

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
    status = WAITING;
    response = null;
  }

  /** Set the sequence number of this command */
  public void setSequenceNumber(int sequenceNumber) {
    this.sequence = sequenceNumber;
  }

  /** Set the response JSON object
   * 
   * @param result The JSON result object from scion-server
   * @param server The server, in case we need to process an asynchronous command's successors
   * @throws JSONException
   */
  public void setResponse(Object result, ScionServer server) throws JSONException {
    response = result;
    // Someone else is waiting for the response and will call processResult().
    synchronized (this) {
      notifyAll();
    }
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
  }

  /** Predicate to test if command is still waiting for a response. */
  public boolean isWaiting() {
    return (status == WAITING);
  }

  /** Predicate to test if the command is in the DONE state. */
  public boolean isDone() {
    return (status == DONE);
  }

  /** Predicate to test if the command has received a response */
  public boolean hasResponse() {
    return (response != null);
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

  public final void processResult() throws JSONException {
    assert (response != null);
    doProcessResult();
  }

  protected void doProcessResult() throws JSONException {
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
