package net.sf.eclipsefp.haskell.scion.exceptions;


public abstract class ScionException extends Exception {

	private static final long serialVersionUID = 1L;
	
	public ScionException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public ScionException(String message) {
		super(message);
	}
	
}
