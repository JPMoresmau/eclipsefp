package net.sf.eclipsefp.haskell.ui.console;

import org.eclipse.ui.console.TextConsole;

@Deprecated
public class FakeCleaner implements IConsoleCleaner {

	public void clean(final TextConsole console) {
		//fake cleaner does not clean anything
	}

}
