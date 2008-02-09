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

import net.sf.eclipsefp.haskell.core.internal.util.MultiplexedWriter;

import org.eclipse.core.resources.IFile;

public class ListenableCompilerDecorator implements IHaskellCompiler {

	private final IHaskellCompiler fUnderlyingCompiler;
	private final MultiplexedWriter fOutWriter = new MultiplexedWriter();
	private final List<ICompilerListener> fListeners =
		new ArrayList<ICompilerListener>();

	public ListenableCompilerDecorator(final IHaskellCompiler compiler) {
		fUnderlyingCompiler = compiler;
	}

	public void addListener(final ICompilerListener listener) {
		fListeners.add(listener);
		fOutWriter.addOutput(listener.getOutputWriter());
	}

	public void removeListener(final ICompilerListener listener) {
		fListeners.remove(listener);
		fOutWriter.removeOutput(listener.getOutputWriter());
	}
	
	public ICompilerOutput compile(final IFile file, final Writer outputWriter) {
		// TODO Auto-generated method stub
		return null;
	}

	public ICompilerOutput compile(final IFile file) {
		for (ICompilerListener listener : fListeners) {
			listener.startingCompilation();
		}
		
		return fUnderlyingCompiler.compile(file, fOutWriter);
	}

}
