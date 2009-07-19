package net.sf.eclipsefp.haskell.scion.exceptions;

import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;

import org.eclipse.osgi.util.NLS;

/**
 * An exception indicating a fatal error in the Scion server or
 * in client/server communication. If this is thrown, the server is
 * guaranteed to be in an unusable state and needs to be restarted.
 * 
 * @author Thomas ten Cate
 */
public class ScionServerException extends ScionException {

	private static final long serialVersionUID = 1L;
	
	private String lastWords;

	public ScionServerException(String message, Throwable cause) {
		super(message, cause);
	}

	public ScionServerException(String message) {
		super(message);
	}
	
	public void setLastWords(String lastWords) {
		this.lastWords = lastWords;
	}
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
		if (getMessage() != null) {
			sb.append(getMessage());
		}
		if (lastWords != null && lastWords.length() > 0) {
			if (sb.length() > 0) {
				sb.append("\n");
			}
			sb.append(NLS.bind(UITexts.scionServerLastWords_message, lastWords));
		}
		return sb.toString();
	}

}
