package net.sf.eclipsefp.haskell.core.compiler;

import java.io.Writer;

import net.sf.eclipsefp.common.core.util.MultiplexedWriter;

import org.eclipse.core.resources.IFile;

public class ListenableCompilerDecorator implements IHaskellCompiler {

	private IHaskellCompiler fUnderlyingCompiler;
	private MultiplexedWriter fOutWriter = new MultiplexedWriter();
	private MultiplexedWriter fErrWriter = new MultiplexedWriter();

	public ListenableCompilerDecorator(IHaskellCompiler compiler) {
		fUnderlyingCompiler = compiler;
	}

	public void addListener(ICompilerListener listener) {
		fOutWriter.addOutput(listener.getOutputWriter());
		fErrWriter.addOutput(listener.getErrorWriter());
	}

	public ICompilerOutput compile(IFile file, Writer outputWriter, Writer errorWriter) {
		// TODO Auto-generated method stub
		return null;
	}

	public ICompilerOutput compile(IFile file) {
		return fUnderlyingCompiler.compile(file, fOutWriter, fErrWriter);
	}
	
}
