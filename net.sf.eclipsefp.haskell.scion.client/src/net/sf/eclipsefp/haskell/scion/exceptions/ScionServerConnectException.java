package net.sf.eclipsefp.haskell.scion.exceptions;

public class ScionServerConnectException extends ScionServerStartupException {

	private static final long serialVersionUID = 1L;

	public ScionServerConnectException(String message, Throwable cause) {
		super(message, cause);
	}

	public ScionServerConnectException(String message) {
		super(message);
	}

}
