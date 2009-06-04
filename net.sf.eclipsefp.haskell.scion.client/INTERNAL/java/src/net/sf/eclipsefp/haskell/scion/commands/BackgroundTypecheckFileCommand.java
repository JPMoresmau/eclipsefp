package net.sf.eclipsefp.haskell.scion.commands;

public class BackgroundTypecheckFileCommand extends ScionCommand {

	private String fileName;
	
	public BackgroundTypecheckFileCommand(String fileName) {
		this.fileName = fileName;
	}
	
	@Override
	protected String internalLisp() {
		// TODO escape quotes
		return String.format("(background-typecheck-file \"%s\")", fileName);
	}

	@Override
	protected void parseInternalResponse(String response) {
		// TODO
	}

}
