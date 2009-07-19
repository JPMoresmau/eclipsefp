package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.osgi.util.NLS;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

/**
 * A command that can be sent to the Scion server.
 * After being run, it can be queried for the response.
 * 
 * @author Thomas ten Cate
 */
public abstract class ScionCommand extends Job {
	
	private final String TO_SERVER_PREFIX = "[scion_server] <<", FROM_SERVER_PREFIX = "[scion_server] >>";

	private int sequenceNumber = 0;
	
	/**
	 * The Scion instance used to run this command.
	 * This also serves as the command's scheduling rule
	 * (preventing multiple commands being run simultaneously).
	 * It also serves as the representative of the command's family.
	 */
	private IScionCommandRunner runner;
	
	/**
	 * Used only for error reporting during response processing.
	 */
	private JSONObject response;

	/**
	 * Constructs a new command.
	 * 
	 * @param runner the command manager that is to be used when running the command
	 * @param priority the job priority; one of INTERACTIVE, SHORT, LONG, BUILD, or DECORATE
	 */
	public ScionCommand(IScionCommandRunner runner, int priority) {
		super("Scion command");
		// can't call getMethod when calling superclass constructor
		// (even this hack is slightly evil, calling subclass methods)
		setName("Scion command `" + getMethod() + "'");
		setRule(runner);
		this.runner = runner;
	}
	
	/**
	 * Schedules this command to be run, and blocks until it is completed.
	 * 
	 * @return the command's completion status
	 */
	public IStatus runSync() {
		schedule();
		while (getState() != NONE) { // alternatives: WAITING, RUNNING or SLEEPING
			try {
				join();
			} catch (InterruptedException e) {
				// re-check state
			}
		}
		return getResult();
	}
	
	/**
	 * Schedules this command to be run at some point in the future.
	 */
	public void runAsync() {
		schedule();
	}
	
	/**
	 * Schedules this command to be run at some time after the given command completes.
	 * Completion can mean either success, failure or cancellation.
	 * 
	 * If the given command is not scheduled (which probably indicates that it's done already)
	 * then the current command is scheduled immediately.
	 * 
	 * @param command the command to wait for
	 */
	public void runAsyncAfter(ScionCommand command) {
		if (command.getState() == Job.NONE) {
			runAsync();
		} else {
			command.addJobChangeListener(new JobChangeAdapter() {
				public void done(IJobChangeEvent event) {
					runAsync();
				}
			});
		}
	}
	
	////////////////////////////////
	// methods overridden from Job
	
	/**
	 * Runs the command, blocking until completion or error.
	 * 
	 * This should not be called; use {@link #runSync()} or {@link #runAsync()}.
	 */
	@Override
	public IStatus run(IProgressMonitor monitor) {
		// Jobs that finish asynchronously must specify the execution thread by calling setThread,
		// and must indicate when they are finished by calling the method done.
		try {
			runner.runCommandSync(this);
		} catch (Exception ex) {
			Trace.trace("Exception when running command", ex);
			IStatus status = new Status(IStatus.ERROR, ScionPlugin.getPluginId(), IStatus.ERROR, ex.getMessage(), ex);
			ScionPlugin.logStatus(status);
		}
		return Status.OK_STATUS;
	}
	
	@Override
	public boolean belongsTo(Object family) {
		return family == runner;
	}
	
	//////////////////////////
	// sending and receiving
	
	/**
	 * Sends the command over the wire to the given output writer.
	 * 
	 * @throws ScionServerException if something happened to the connection
	 */
	public void sendCommand(Writer out) throws ScionServerException {
		String jsonString = toJSONString();
		
		Trace.trace(TO_SERVER_PREFIX, "%s", jsonString);

		try {
			out.write(jsonString);
			out.write("\n");
			out.flush();
		} catch (IOException ex) {
			throw new ScionServerException(UITexts.scionServerConnectionError_message, ex);
		}
	}
	
	/**
	 * Waits for the command response over the wire from the given input reader.
	 * 
	 * @throws ScionServerException if the response could not be read from the server
	 * @throws ScionCommandException if something went wrong when processing
	 */
	public void receiveResponse(Reader reader) throws ScionCommandException, ScionServerException {
		JSONObject response;
		try {
			response = new JSONObject(new JSONTokener(reader));
		} catch (JSONException ex) {
			// we throw a server exception, because there's no telling what state the
			// server is in after we've received a malformed response (or end-of-stream!)
			throw new ScionServerException(UITexts.scionJSONParseException_message, ex);
		}
		Trace.trace(FROM_SERVER_PREFIX, "%s", response.toString());
		processResponse(response);
	}

	///////////////
	// JSON stuff
	
	public void setSequenceNumber(int sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}
	
	/**
	 * Serializes the command to its JSON equivalent, ready to be sent to the server.
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
			json.put("id", sequenceNumber);
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
	 * Returns the name of the "method" to be called on the server side, e.g. "connection-info".
	 */
	protected abstract String getMethod();
	
	/**
	 * Creates the params JSON object to be sent along with the command.
	 * The default implementation returns an empty map; most subclasses will want to override this.
	 * Must not return null.
	 */
	protected JSONObject getParams() throws JSONException {
		return new JSONObject();
	}
	
	/**
	 * Parses the given response string and stores the command result in this object.
	 * 
	 * @throws ScionCommandException if something went wrong
	 */
	public void processResponse(JSONObject response) throws ScionCommandException {
		this.response = response;
		try {
			checkResponseVersion(response);
			checkResponseId(response);
			processResponseResult(response);
		} finally {
			this.response = null;
		}
	}

	private void processResponseResult(JSONObject response) throws ScionCommandException {
		Object result;
		try {
			result = response.get("result");
		} catch (JSONException ex) {
			try {
				JSONObject error = response.getJSONObject("error");
				String name = error.getString("name");
				String message = error.getString("message");
				throw new ScionCommandException(this, NLS.bind(UITexts.commandError_message, name, message), ex);
			} catch (JSONException ex2) {
				throw new ScionCommandException(this, UITexts.commandErrorMissing_message, ex2);
			}
		}
		try {
			doProcessResult(result);
		} catch (JSONException ex) {
			throw new ScionCommandException(this, UITexts.commandProcessingFailed_message, ex);
		}
	}

	private void checkResponseId(JSONObject response) {
		try {
			int id = response.getInt("id");
			if (id != sequenceNumber) {
				ScionPlugin.logWarning(this, NLS.bind(UITexts.commandIdMismatch_warning, Integer.toString(id), Integer.toString(sequenceNumber)), null);
			}
		} catch (JSONException ex) {
			ScionPlugin.logWarning(this, UITexts.errorReadingId_warning, ex);
		}
	}

	private void checkResponseVersion(JSONObject response) {
		try {
			String version = response.getString("version");
			String expectedVersion = "0.1";
			if (!version.equals(expectedVersion)) {
				ScionPlugin.logWarning(this, NLS.bind(UITexts.commandVersionMismatch_warning, version, expectedVersion), null);
			}
		} catch (JSONException ex) {
			ScionPlugin.logWarning(this, UITexts.errorReadingVersion_warning, ex);
		}
	}
	
	protected abstract void doProcessResult(Object result) throws JSONException;
	
	/**
	 * Returns information about this command for use in error messages.
	 * Guaranteed never to throw exceptions, so safe for use in exception handling.
	 * 
	 * @return information about this command; never <code>null</code>
	 */
	public String getErrorInfo() {
		String info = null;
		try {
			info = NLS.bind(UITexts.scionFailedCommand_message, toString());
			if (this.response != null) {
				info += "\n" + NLS.bind(UITexts.scionFailedResponse_message, prettyPrint(response));
			}
		} catch (Throwable ex) {
			// ignore
		}
		if (info == null) {
			return "";
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
	
}
