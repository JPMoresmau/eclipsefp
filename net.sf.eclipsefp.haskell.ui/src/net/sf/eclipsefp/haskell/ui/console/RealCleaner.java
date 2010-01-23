package net.sf.eclipsefp.haskell.ui.console;

import org.eclipse.ui.console.TextConsole;
@Deprecated
public class RealCleaner implements IConsoleCleaner {

	public void clean(final TextConsole console) {
		console.clearConsole();
	}

}
