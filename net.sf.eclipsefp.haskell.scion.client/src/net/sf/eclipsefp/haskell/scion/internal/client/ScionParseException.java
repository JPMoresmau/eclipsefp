package net.sf.eclipsefp.haskell.scion.internal.client;

public class ScionParseException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public ScionParseException() {
	}

	public ScionParseException(String message) {
		super(message);
	}

	public ScionParseException(Throwable cause) {
		super(cause);
	}

	public ScionParseException(String message, Throwable cause) {
		super(message, cause);
	}

}
