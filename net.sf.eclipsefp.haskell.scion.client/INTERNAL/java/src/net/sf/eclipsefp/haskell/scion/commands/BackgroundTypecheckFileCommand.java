package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.lisp.LispExpr;

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
	protected void parseInternalResponse(LispExpr response) {
		// if success:
		// (:ok (:ok (t (compilation-result t nil 0.008985))))
		// if error:
		// TODO

	}

}
