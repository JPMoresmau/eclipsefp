package net.sf.eclipsefp.haskell.scion.internal.commands;

public class ModuleGraphCommand extends DefinedNamesCommand {
	public ModuleGraphCommand() {
		super();
	}

	@Override
  public String getMethod() {
		return "module-graph";
	}
}
