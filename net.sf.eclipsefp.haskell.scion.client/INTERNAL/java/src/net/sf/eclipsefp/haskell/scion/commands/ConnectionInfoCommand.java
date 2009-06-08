package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.lisp.LispExpr;


public class ConnectionInfoCommand extends ScionCommand {

	public String internalLisp() {
		return "(connection-info)";
	}
	
	public void parseInternalResponse(LispExpr response) {
		// TODO
	}

}
