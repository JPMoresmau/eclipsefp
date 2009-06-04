package net.sf.eclipsefp.haskell.scion.commands;


public class ThingAtPointCommand extends ScionCommand {

	private String fileName;
	private int line, column;
	
	private String thing; // the response
	
	public ThingAtPointCommand(String file, int line, int column) {
		this.fileName = file;
		this.line = line;
		this.column = column;
	}
	
	@Override
	protected String internalLisp() {
		// TODO escape quotes
		return String.format("(thing-at-point \"%s\" %d %d)", fileName, line + 1, column);
	}

	@Override
	protected void parseInternalResponse(String response) {
		// TODO really quick and dirty!
		// either (:ok (:ok nil))
		//     or (:ok (:ok "some_string"))
		int begin = response.indexOf('(', 1);
		begin = response.indexOf(' ', begin) + 1;
		int end = response.lastIndexOf(')', response.length() - 2);
		response = response.substring(begin, end);
		if (response.charAt(0) == '"') {
			// quoted string
			thing = response.substring(1, response.length() - 1);
		} else {
			// probably nil
			thing = null;
		}
	}
	
	public String getThing() {
		return thing;
	}

}
