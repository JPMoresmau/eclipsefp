package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionParseException;
import net.sf.eclipsefp.haskell.scion.lisp.LispExpr;
import net.sf.eclipsefp.haskell.scion.lisp.LispList;

public class NameDefinitionsCommand extends ScionCommand {

	private String name;
	
	private boolean found = false;
	private String file;
	private int startLine, startColumn, endLine, endColumn;
	
	public NameDefinitionsCommand(String name) {
		this.name = name;
	}
	
	@Override
	protected String internalLisp() {
		return String.format("(name-definitions \"%s\")", name);
	}

	@Override
	protected void parseInternalResponse(LispExpr response) {
		// if found:
		// (:ok ((:loc "/home/thomas/gsoc/runtime-workspace/hello-world/src/HelloWorld.hs" 15 0 15 1)))
		// otherwise:
		// (:ok nil)
		try {
			LispList list = response.asList().get(1).asList();
			if (list.length() >= 1) {
				// one or more results
				// TODO what if more than one?
				LispList pos = list.get(0).asList();
				file = pos.get(1).asString().getValue();
				startLine = pos.get(2).asNumber().getInt();
				startColumn = pos.get(3).asNumber().getInt();
				endLine = pos.get(4).asNumber().getInt();
				endColumn = pos.get(5).asNumber().getInt();
				found = true;
			} else {
				// no results
				found = false;
			}
		} catch (ScionParseException ex) {
			found = false;
		}
	}

	public boolean isFound() {
		return found;
	}
	
	public String getFileName() {
		return file;
	}

	public int getStartLine() {
		return startLine;
	}

	public int getStartColumn() {
		return startColumn;
	}

	public int getEndLine() {
		return endLine;
	}

	public int getEndColumn() {
		return endColumn;
	}

}
