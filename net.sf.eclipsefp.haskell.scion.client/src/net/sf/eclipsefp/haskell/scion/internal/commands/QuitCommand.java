package net.sf.eclipsefp.haskell.scion.internal.commands;

public class QuitCommand extends ScionCommand {

	public QuitCommand() {
		super();
	}

	@Override
  public String getMethod() {
		return "quit";
	}

}
