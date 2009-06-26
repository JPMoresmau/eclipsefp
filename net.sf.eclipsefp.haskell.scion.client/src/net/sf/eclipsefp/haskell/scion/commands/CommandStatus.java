package net.sf.eclipsefp.haskell.scion.commands;

public enum CommandStatus {
	
	/**
	 * The initial state the command is in when it is created.
	 */
	NEW,
	/**
	 * The command is waiting in the command queue, but has not been sent to the server yet.
	 */
	ENQUEUED,
	/**
	 * The command could not be enqueued, because the queue was full.
	 */
	QUEUE_FULL,
	/**
	 * The command has been sent off to the server.
	 */
	RUNNING,
	/**
	 * The command completed successfully and now contains its response.
	 */
	SUCCESS,
	/**
	 * Running the command failed.
	 */
	FAILED,
	/**
	 * The command was sent to the server but there was a timeout waiting for the response.
	 */
	TIMEOUT;
	
	/**
	 * Returns true if the status of the command will not change hereafter.
	 */
	public boolean isFinished() {
		return this == QUEUE_FULL || this == SUCCESS || this == FAILED || this == TIMEOUT;
	}

}
