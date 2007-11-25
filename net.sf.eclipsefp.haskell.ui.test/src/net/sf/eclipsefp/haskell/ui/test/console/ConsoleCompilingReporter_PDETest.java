/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui.test.console;

import static org.easymock.EasyMock.*;
import org.eclipse.ui.console.TextConsole;

import net.sf.eclipsefp.haskell.ui.console.ConsoleCompilingReporter;
import net.sf.eclipsefp.haskell.ui.console.IConsoleCleaner;
import junit.framework.TestCase;

public class ConsoleCompilingReporter_PDETest extends TestCase {
	
	public void testAskCleanerToCleanConsoleWhenStartingCompilation() {
		IConsoleCleaner cleaner = createMock(IConsoleCleaner.class);
		cleaner.clean((TextConsole) anyObject());
		expectLastCall().once();
		replay(cleaner);
		
		ConsoleCompilingReporter reporter =
			new ConsoleCompilingReporter(cleaner);
		reporter.startingCompilation();
		
		verify(cleaner);
	}

}
