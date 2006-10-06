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
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.common.core.util.MultiplexedWriter;

import org.eclipse.core.resources.IFile;

public class ListenableCompilerDecorator implements IHaskellCompiler {

	private IHaskellCompiler fUnderlyingCompiler;
	private MultiplexedWriter fOutWriter = new MultiplexedWriter();
	private MultiplexedWriter fErrWriter = new MultiplexedWriter();
	private List<ICompilerListener> fListeners =
		new ArrayList<ICompilerListener>();

	public ListenableCompilerDecorator(IHaskellCompiler compiler) {
		fUnderlyingCompiler = compiler;
	}

	public void addListener(ICompilerListener listener) {
		fListeners.add(listener);
		fOutWriter.addOutput(listener.getOutputWriter());
		fErrWriter.addOutput(listener.getErrorWriter());
	}

	public ICompilerOutput compile(IFile file, Writer outputWriter, Writer errorWriter) {
		// TODO Auto-generated method stub
		return null;
	}

	public ICompilerOutput compile(IFile file) {
		for (ICompilerListener listener : fListeners) {
			listener.startingCompilation();
		}
		
		return fUnderlyingCompiler.compile(file, fOutWriter, fErrWriter);
	}
	
}
