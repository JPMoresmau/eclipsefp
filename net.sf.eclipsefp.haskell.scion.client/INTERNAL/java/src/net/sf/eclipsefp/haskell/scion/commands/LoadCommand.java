package net.sf.eclipsefp.haskell.scion.commands;

public class LoadCommand extends ScionCommand {

	private String fileName;
	
	public LoadCommand(String fileName) {
		this.fileName = fileName;
	}
	
	@Override
	protected String internalLisp() {
		// TODO escape quotes
		return String.format("(load (:file \"%s\"))", fileName);
	}

	@Override
	protected void parseInternalResponse(String response) {
		// TODO
	}

}
