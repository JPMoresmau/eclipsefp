package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.ICommandContinuation;
import net.sf.eclipsefp.haskell.scion.internal.servers.GenericScionServer;
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
  private int                      sequence;
  /** Commands to be executed after this command */
  private final List<ScionCommand> successors;
  /** Methods to be run after the command has finished executing. */
  private final List<ICommandContinuation> continuations;
  /**
   * Used only for error reporting during response processing.
   */
  private JSONObject               response;
  /**
   * Constructs a new scion-server command.
   * 
   * @param runner
   *          the command manager that is to be used when running the command
   * @param priority
   *          the job priority; one of INTERACTIVE, SHORT, LONG, BUILD, or
   *          DECORATE
   */
  public ScionCommand() {
    successors = new LinkedList<ScionCommand>();
    continuations = new LinkedList<ICommandContinuation>();
  }
  
  /** Set the continuation that needs to be run when the job's status changes */
  public void addContinuation( final ICommandContinuation continuation ) {
    continuations.add(continuation);
  }
  
  /** Set the sequence number of this command */
  public void setSequenceNumber( int sequenceNumber ) {
    this.sequence = sequenceNumber;
  }
  
  /** Set the response JSON object */
  public void setResponse( JSONObject response ) {
    this.response = response;
  }
  
  /**
   * Schedules this command to be run at some time after the given command
   * completes. Completion can mean either success, failure or cancellation.
   * 
   * If the given command is not scheduled (which probably indicates that it's
   * done already) then the current command is scheduled immediately.
   * 
   * @param command
   *          the command to wait for
   */
  public final void runAsyncAfter(ScionCommand command) {
    command.addSuccessor(this);
  }

  public boolean runSuccessors(GenericScionServer server) {
    boolean retval = true;
    for (ScionCommand sc : successors) {
      boolean completion = server.sendCommandSync(sc);
      
      retval &= completion;
    }
    
    return retval;
  }

  public boolean onError(JSONException ex, String name, String message) {
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
   *  @return a non-null JSONObject().
   */
  protected JSONObject getParams() throws JSONException {
    return new JSONObject();
  }

  public void processResult(Object result) throws JSONException {
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

  protected static String prettyPrint(JSONObject json) {
    try {
      return json.toString(2);
    } catch (JSONException e) {
      // strangely, the not pretty-printed version does not throw at all
      return json.toString();
    }
  }

  /** Add a successor to this command that is executed once this command completets
   * successfully.
   * 
   * @param successor The following command to execute.
   */
  public void addSuccessor(ScionCommand successor) {
    successors.add(successor);
  }
}
