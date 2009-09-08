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

import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;

public class ConsoleCompilingReporter extends HaskellConsole implements ICompilerListener {

	public ConsoleCompilingReporter(final IConsoleCleaner cleaner) {
		super(cleaner,"GHC Compiler Output");
	}

	public void startingCompilation() {
		getCleaner().clean(getConsole());
	}

}
