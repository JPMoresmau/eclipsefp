package net.sf.eclipsefp.haskell.scion.exceptions;

public class ScionServerStartupException extends ScionServerException {

	private static final long serialVersionUID = 1L;

	public ScionServerStartupException(String message, Throwable cause) {
		super(message, cause);
	}

	public ScionServerStartupException(String message) {
		super(message);
	}

}
