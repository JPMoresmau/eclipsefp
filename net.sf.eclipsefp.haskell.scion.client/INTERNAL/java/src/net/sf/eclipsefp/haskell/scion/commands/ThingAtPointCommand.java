package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.lisp.LispExpr;
import net.sf.eclipsefp.haskell.scion.lisp.LispList;
import net.sf.eclipsefp.haskell.scion.lisp.LispString;


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
		LispExpr inner = ((LispList)((LispList)response).get(1)).get(1);
		if (inner instanceof LispString) {
			thing = ((LispString)inner).getValue();
			if (thing == "no info")
				thing = null;
		} else {
			thing = null;
		}
	}
	
	public String getThing() {
		return thing;
	}

}
