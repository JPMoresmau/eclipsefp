package net.sf.eclipsefp.haskell.scion.commands;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.ScionClientException;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * A command that can be sent to the Scion server.
 * After being run, it can be queried for the response.
 * 
 * @author Thomas ten Cate
 */
public abstract class ScionCommand {
	
	private volatile CommandStatus status = CommandStatus.NEW;
	private int sequenceNumber = 0;
	private List<IScionCommandFinishedListener> finishedListeners;

	public void setSequenceNumber(int sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}
	
	/**
	 * Serializes the command to its JSON equivalent, ready to be sent to the server.
	 * 
	 * @return a valid JSON string
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
	
	/**
	 * Sets the status of this command.
	 */
	public void setStatus(CommandStatus status) {
		boolean fireFinishedEvent = !this.status.isFinished() && status.isFinished();
		this.status = status;
		if (fireFinishedEvent) {
			fireFinishedEvent();
		}
	}
	
	public CommandStatus getStatus() {
		return status;
	}
	
	public boolean isSuccessful() {
		return status == CommandStatus.SUCCESS;
	}
	
	public void addFinishedListener(IScionCommandFinishedListener listener) {
		if (finishedListeners == null) {
			finishedListeners = new ArrayList<IScionCommandFinishedListener>();
		}
		finishedListeners.add(listener);
	}
	
	private void fireFinishedEvent() {
		if (finishedListeners != null) {
			for (IScionCommandFinishedListener listener : finishedListeners) {
				listener.onScionCommandFinished(this);
			}
		}
	}
	
}
