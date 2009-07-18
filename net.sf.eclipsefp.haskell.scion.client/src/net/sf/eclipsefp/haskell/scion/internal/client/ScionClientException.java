package net.sf.eclipsefp.haskell.scion.internal.client;

public class ScionClientException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public ScionClientException() {
		super();
	}

	public ScionClientException(String message, Throwable cause) {
		super(message, cause);
	}

	public ScionClientException(String message) {
		super(message);
	}

	public ScionClientException(Throwable cause) {
		super(cause);
	}
	
}
