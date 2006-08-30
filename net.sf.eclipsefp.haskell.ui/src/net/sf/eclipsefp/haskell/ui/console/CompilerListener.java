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

public class CompilerListener implements ICompilerListener {

	private OutputStreamWriter fErrorWriter;
	private OutputStreamWriter fOutputWriter;

	public CompilerListener() {
		IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
		IOConsole console = new IOConsole("GHC Compiler Output", null);
		mgr.addConsoles(new IConsole[] {console});
		
		IOConsoleOutputStream outputStream = console.newOutputStream();
		IOConsoleOutputStream errorStream = console.newOutputStream();
		outputStream.setColor(new Color(HaskellUIPlugin.getStandardDisplay(), 0, 0, 255));
		errorStream.setColor(new Color(HaskellUIPlugin.getStandardDisplay(), 255, 0, 0));
		
		fOutputWriter = new OutputStreamWriter(outputStream);
		fErrorWriter = new OutputStreamWriter(errorStream);
	}
	
	public Writer getErrorWriter() {
		return fErrorWriter;
	}

	public Writer getOutputWriter() {
		return fOutputWriter;
	}

}
