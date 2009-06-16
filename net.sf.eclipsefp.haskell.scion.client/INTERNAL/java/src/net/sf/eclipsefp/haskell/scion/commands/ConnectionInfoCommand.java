package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.lisp.LispExpr;


public class ConnectionInfoCommand extends ScionCommand {

	@Override
	public String internalLisp() {
		return "(connection-info)";
	}
	
	@Override
	public void parseInternalResponse(LispExpr response) {
		// TODO
	}

}
