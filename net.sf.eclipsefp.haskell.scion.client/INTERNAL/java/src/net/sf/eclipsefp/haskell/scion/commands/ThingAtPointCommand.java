package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionParseException;
import net.sf.eclipsefp.haskell.scion.lisp.LispExpr;


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
	protected void parseInternalResponse(LispExpr response) {
		// either (:ok (:ok nil))
		//     or (:ok (:ok "some_string"))
		try {
			thing = response.asList().get(1).asList().get(1).asString().getValue();
			if (thing.equals("no info"))
				thing = null;
		} catch (ScionParseException ex) {
			thing = null;
		}
	}
	
	public String getThing() {
		return thing;
	}

}
