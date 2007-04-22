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
package net.sf.eclipsefp.haskell.core.compiler;

import java.io.Writer;

import org.eclipse.core.resources.IFile;

/**
 * <p>Default implementation for an IHaskellCompiler. This class can be used
 * as a base for other implementations.</p>
 * 
 * <p>The default {@link #compile(IFile, Writer, Writer)} does no compiling
 * and returns an almost empty output. Subclasses are supposed to override it to
 * provide implementation specific behaviour.</p>
 * 
 * @author Leif Frenzel  
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class DefaultHaskellCompiler implements IHaskellCompiler {

	public ICompilerOutput compile(IFile file, Writer outputWriter) {
		return new DefaultHaskellCompilerOutput(file.getName());
	}

	public ICompilerOutput compile(IFile file) {
		return compile(file, new NullWriter());
	}

}
