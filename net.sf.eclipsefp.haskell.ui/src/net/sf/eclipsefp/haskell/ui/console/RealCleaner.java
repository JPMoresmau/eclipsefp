package net.sf.eclipsefp.haskell.ui.console;

import org.eclipse.ui.console.TextConsole;

public class RealCleaner implements IConsoleCleaner {

	public void clean(TextConsole console) {
		console.clearConsole();
	}

}
