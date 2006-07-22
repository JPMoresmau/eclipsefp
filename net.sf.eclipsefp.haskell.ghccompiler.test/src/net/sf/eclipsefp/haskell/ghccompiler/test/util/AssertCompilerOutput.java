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
package net.sf.eclipsefp.haskell.ghccompiler.test.util;

import java.util.Collection;

import static junit.framework.Assert.*;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;

/**
 * Assertions for compiler output
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class AssertCompilerOutput {
	
	public static void assertContains(int line, int startColumn, int endColumn,
		    String message, Collection<ICompilerOutputItem> errors)
		{
			for(ICompilerOutputItem item : errors) {
				if (   line == item.getLine()
				   && startColumn == item.getStartColumn()
				   && endColumn == item.getEndColumn()
				   && message.equals(item.getComment()) )
				{
					return;
				}
			}
			fail(String.format("Could not find error on line %d, range (%d, %d) " +
					           "and message '%s'", line, startColumn, endColumn,
					           message));
		}
	
}
