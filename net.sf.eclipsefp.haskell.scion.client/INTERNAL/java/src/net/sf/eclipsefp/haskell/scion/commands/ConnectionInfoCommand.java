package net.sf.eclipsefp.haskell.scion.commands;


public class ConnectionInfoCommand extends ScionCommand {

	public String internalLisp() {
		return "(connection-info)";
	}
	
	public void parseInternalResponse(String response) {
		// TODO
	}

}
