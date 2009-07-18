package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.client.ScionClientException;
import net.sf.eclipsefp.haskell.scion.internal.client.ScionThreadManager;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A command that can be sent to the Scion server.
 * After being run, it can be queried for the response.
 * 
 * @author Thomas ten Cate
 */
public abstract class ScionCommand extends Job {
	
	private int sequenceNumber = 0;
	
	/**
	 * The command manager used to run this command.
	 * This also serves as the command's scheduling rule
	 * (preventing multiple commands being run simultaneously).
	 * It also serves as the representative of the command's family.
	 */
	private ScionThreadManager manager;

	/**
	 * Constructs a new command.
	 * 
	 * @param manager the command manager that is to be used when running the command
	 * @param priority the job priority; one of INTERACTIVE, SHORT, LONG, BUILD, or DECORATE
	 */
	public ScionCommand(ScionThreadManager manager, int priority) {
		super("Scion command");
		// can't call getMethod when calling superclass constructor
		// (even this hack is slightly evil)
		setName("Scion command `" + getMethod() + "'");
		setRule(manager);
		this.manager = manager;
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
	 * @param command the command to wait for
	 */
	public void runAsyncAfter(ScionCommand command) {
		command.addJobChangeListener(new JobChangeAdapter() {
			public void done(IJobChangeEvent event) {
				runAsync();
			}
		});
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
		sequenceNumber = manager.makeSequenceNumber();
		try {
			manager.runCommandSync(this);
		} catch (Exception ex) {
			Trace.trace("Exception when running command", ex);
			return new Status(IStatus.ERROR, ScionPlugin.getPluginId(), IStatus.ERROR, ex.getMessage(), ex);
		}
		return Status.OK_STATUS;
	}
	
	@Override
	public boolean belongsTo(Object family) {
		return family == manager;
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
	 * 
	 * TODO rename, at call sites too!
	 */
	@Override
	public String toString() {
		JSONObject command = new JSONObject();
		try {
			command.put("method", getMethod());
			command.put("params", getParams());
			command.put("id", sequenceNumber);
		} catch (JSONException e) {
			// should not happen
		}
		return command.toString();
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
	 * @throws ScionClientException if something went wrong
	 * 
	 * TODO: error messages could be more informative: carry inner JSONException along,
	 * show the message and the response, list possible causes/solutions 
	 */
	public void processResponse(JSONObject response) {
		// check version
		String version;
		try {
			version = response.getString("version");
		} catch (JSONException e) {
			throw new ScionClientException(String.format("Could not get a valid response version from the Scion response"));
		}
		if (!version.equals("0.1")) {
			throw new ScionClientException(String.format("The Scion response has version %s, but only version 0.1 is supported", version));
		}
		
		// check sequence number
		int id;
		try {
			id = response.getInt("id");
		} catch (JSONException ex) {
			throw new ScionClientException(String.format("Could not get a valid command id from the Scion response"));
		}
		if (id != sequenceNumber) {
			throw new ScionClientException(String.format("Command with id %d got a response with id %d.", sequenceNumber, id));
		}
		
		// process result
		Object result;
		try {
			result = response.get("result");
		} catch (JSONException ex) {
			try {
				JSONObject error = response.getJSONObject("error");
				String name = error.getString("name");
				String message = error.getString("message");
				throw new ScionClientException(String.format("The Scion server returned an error of type %s:\n%s", name, message));
			} catch (JSONException ex2) {
				throw new ScionClientException(String.format("The Scion server returned no result, but no error either"));
			}
		}
		try {
			processResult(result);
		} catch (JSONException e) {
			throw new ScionClientException(String.format("Could not process command result"));
		}
	}
	
	protected abstract void processResult(Object result) throws JSONException;
	
}
