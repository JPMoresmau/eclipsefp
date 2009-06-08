package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.lisp.LispExpr;

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
	protected void parseInternalResponse(LispExpr response) {
		// if success:
		// (:ok (:ok (compilation-result t nil 0.152692)))
		// if error:
		// (:ok (:ok (compilation-result nil
		//            ((:error (:loc "/home/thomas/gsoc/runtime-workspace/hello-world/src/HelloWorld.hs" 8.0 10.0 8.0 26.0)
		//              "Couldn't match expected type `Int' against inferred type `[Char]'"))
		//            0.236684)))
		// TODO
	}

}
