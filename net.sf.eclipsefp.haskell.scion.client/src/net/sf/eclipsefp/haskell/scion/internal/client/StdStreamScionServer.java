package net.sf.eclipsefp.haskell.scion.internal.client;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Implementation of IScion server using standard in/out to communicate with Scion
 * @author JP Moresmau
 *
 */
public class StdStreamScionServer extends AbstractScionServer {
	private static final String PREFIX="scion:";
	private BufferedWriter serverStdInWriter;
	
	public StdStreamScionServer(IPath serverExecutable,Writer serverOutput,File directory) {
		super(serverExecutable, serverOutput, directory);
	}
	
	public void runCommandSync(ScionCommand command, IProgressMonitor monitor)
			throws ScionServerException, ScionCommandException {
		command.setSequenceNumber(makeSequenceNumber());
		try {
			command.sendCommand(serverStdInWriter,monitor);
			String line=readLineFromServer();
			while (line!=null && !line.startsWith(PREFIX)){
				line=readLineFromServer();
			} 
			if (line!=null){
				command.receiveResponse(new StringReader(line.substring(PREFIX.length())), monitor);
			}
		} catch (IOException ioe){
			throw new ScionServerException(ioe.getLocalizedMessage(),ioe);
		}
	}

	public void startServer() throws ScionServerStartupException {
		List<String> command = new LinkedList<String>();
		command.add(serverExecutable.toOSString());
		command.add("-i ");
		
		// Launch the process
		ProcessBuilder builder = new ProcessBuilder(command);
		if (directory!=null && directory.exists()){
			builder.directory(directory);
		}
		builder.redirectErrorStream(true); // send server's stderr to its stdout
		try {
			process = builder.start();
		} catch (Throwable ex) {
			throw new ScionServerStartupException(UITexts.scionServerCouldNotStart_message, ex);
		}
		
		// Connect to the process's stdout to capture messages
		// Assume that status messages and such will be UTF8 only
		try {
			serverStdOutReader = new BufferedReader(new InputStreamReader(process.getInputStream(), "UTF8"));
		} catch (UnsupportedEncodingException ex) {
			// make compiler happy, because UTF8 is always supported
		}
		try {
			serverStdInWriter= new BufferedWriter(new OutputStreamWriter(process.getOutputStream(), "UTF8"));
		} catch (UnsupportedEncodingException ex) {
			// make compiler happy, because UTF8 is always supported
		}
		Trace.trace(CLASS_PREFIX, "Server started");
	}

	public void stopServer() {
		Trace.trace(CLASS_PREFIX, "Stopping server");
		
		if (serverStdOutReader != null) {
			try {
				serverStdOutReader.close();
				serverStdOutReader = null;
			} catch (Throwable ex) {
				// ignore
			}
		}

		if (serverStdInWriter != null) {
			try {
				serverStdInWriter.close();
				serverStdInWriter = null;
			} catch (Throwable ex) {
				// ignore
			}
		}

		if (process != null) {
			process.destroy();
			process = null;
		}
		
		Trace.trace(CLASS_PREFIX, "Server stopped");

	}

}
