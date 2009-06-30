/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Leif Frenzel - Initial API and implementation
 *     Thiago Arrais - Reestructuring and documentation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui.console;

import java.io.OutputStreamWriter;
import java.io.Writer;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.swt.graphics.Color;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;

public class ConsoleCompilingReporter implements ICompilerListener {

	private IOConsole fConsole;
	private final IConsoleCleaner fCleaner;

	public ConsoleCompilingReporter(final IConsoleCleaner cleaner) {
		fCleaner = cleaner;
		createIOConsole();
	}

	private void createIOConsole() {
		IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
		fConsole = new IOConsole("GHC Compiler Output", null);
		mgr.addConsoles(new IConsole[] {fConsole});
	}

	public Writer createOutputWriter() {
    final IOConsoleOutputStream outputStream = fConsole.newOutputStream();
    HaskellUIPlugin.getStandardDisplay().syncExec( new Runnable() {
      public void run() {
        outputStream.setColor(new Color(HaskellUIPlugin.getStandardDisplay(), 0, 0, 255));
      }
    });
    Writer outputWriter = new OutputStreamWriter(outputStream);
    return outputWriter;
	}

	public void startingCompilation() {
		fCleaner.clean(fConsole);
	}

}
