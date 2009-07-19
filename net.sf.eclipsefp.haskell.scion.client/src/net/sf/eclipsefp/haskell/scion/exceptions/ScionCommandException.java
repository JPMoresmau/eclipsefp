package net.sf.eclipsefp.haskell.scion.exceptions;

import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

public class ScionCommandException extends ScionException {

	private static final long serialVersionUID = 1L;

	private ScionCommand command;

	public ScionCommandException(ScionCommand command, String message, Throwable cause) {
		super(constructMessage(command, message), cause);
	}

	public ScionCommandException(ScionCommand command, String message) {
		super(constructMessage(command, message));
	}

	private static String constructMessage(ScionCommand command, String message) {
		if (command == null) {
			return message;
		} else {
			String cmd = command.getErrorInfo();
			if (message == null) {
				return cmd;
			} else {
				return message + "\n" + cmd;
			}
		}
	}
	
	public ScionCommand getCommand() {
		return command;
	}
	
}
