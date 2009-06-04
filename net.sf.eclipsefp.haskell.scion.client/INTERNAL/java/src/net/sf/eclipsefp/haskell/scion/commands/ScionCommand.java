package net.sf.eclipsefp.haskell.scion.commands;

/**
 * A command that can be sent to the Scion server.
 * After being run, it can be queried for the response.
 * 
 * @author Thomas ten Cate
 */
public abstract class ScionCommand {
	
	private boolean completed = false;
	private int sequenceNumber = 0;

	public void setSequenceNumber(int sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}
	
	/**
	 * Serializes the command to its Lisp equivalent, ready to be sent to the server.
	 * 
	 * @return a string of the form "(:emacs-rex some-command sequence-number)"
	 */
	public String toLisp() {
		StringBuffer lisp = new StringBuffer();
		lisp.append("(:emacs-rex ");
		lisp.append(internalLisp());
		lisp.append(" ");
		lisp.append(Integer.toString(sequenceNumber));
		lisp.append(")");
		return lisp.toString();
	}
	
	protected abstract String internalLisp();
	
	public void receiveResponse(String response) {
		// TODO this is really quick and dirty, just to get it working ASAP
		int begin = response.indexOf('(', 1);
		int end = response.lastIndexOf(')', response.length() - 3) + 1; 
		parseInternalResponse(response.substring(begin, end));
		
		complete();
	}
	
	protected abstract void parseInternalResponse(String response);
	
	public synchronized void complete() {
		completed = true;
		notifyAll();
	}
	
	public boolean isCompleted() {
		return completed;
	}
	
}
