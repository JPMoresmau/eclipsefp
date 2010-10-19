package net.sf.eclipsefp.haskell.scion.internal.commands;

public class QuitCommand extends ScionCommand {

	public QuitCommand() {
		super();
	}

	@Override
	protected String getMethod() {
		return "quit";
	}

}
