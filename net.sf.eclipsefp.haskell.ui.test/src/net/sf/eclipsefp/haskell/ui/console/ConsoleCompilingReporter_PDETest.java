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
package net.sf.eclipsefp.haskell.ui.console;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import junit.framework.TestCase;
import org.eclipse.ui.console.TextConsole;

public class ConsoleCompilingReporter_PDETest extends TestCase {

	public void testAskCleanerToCleanConsoleWhenStartingCompilation() {
		IConsoleCleaner cleaner = createMock(IConsoleCleaner.class);
		cleaner.clean((TextConsole) anyObject());
		expectLastCall().once();
		replay(cleaner);
		verify(cleaner);
	}

}
