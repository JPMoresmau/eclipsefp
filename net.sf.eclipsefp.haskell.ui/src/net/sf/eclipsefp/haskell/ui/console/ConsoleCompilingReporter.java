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

import org.eclipse.swt.graphics.Color;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

public class ConsoleCompilingReporter implements ICompilerListener {

	private OutputStreamWriter fOutputWriter;
	private IOConsole fConsole;
	private IConsoleCleaner fCleaner;

	public ConsoleCompilingReporter(IConsoleCleaner cleaner) {
		fCleaner = cleaner;
		createIOStream();
	}

	private void createIOStream() {
		IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
		fConsole = new IOConsole("GHC Compiler Output", null);
		mgr.addConsoles(new IConsole[] {fConsole});
		
		IOConsoleOutputStream outputStream = fConsole.newOutputStream();
		outputStream.setColor(new Color(HaskellUIPlugin.getStandardDisplay(), 0, 0, 255));
		
		fOutputWriter = new OutputStreamWriter(outputStream);
	}

	public Writer getOutputWriter() {
		return fOutputWriter;
	}

	public void startingCompilation() {
		fCleaner.clean(fConsole);
	}

}
