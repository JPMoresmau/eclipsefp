package net.sf.eclipsefp.haskell.scion.client;

public class ScionServerException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public ScionServerException() {
		super();
	}

	public ScionServerException(String message, Throwable cause) {
		super(message, cause);
	}

	public ScionServerException(String message) {
		super(message);
	}

	public ScionServerException(Throwable cause) {
		super(cause);
	}
	
}
