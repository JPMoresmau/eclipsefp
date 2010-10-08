package net.sf.eclipsefp.haskell.scion.internal.client;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.concurrent.atomic.AtomicInteger;

import net.sf.eclipsefp.haskell.scion.internal.util.Trace;

import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Display;

/**
 * some helper code to implement a IScionServer
 * @author JP Moresmau
 *
 */
public abstract class AbstractScionServer implements IScionServer {
	protected static final String
		CLASS_PREFIX = "[ScionServer]",
		SERVER_STDOUT_PREFIX = "[scion-server]";

	protected IPath serverExecutable;
	protected Writer serverOutput;
	
	protected File directory;
	
	protected Process process;
	protected BufferedReader serverStdOutReader;

	private AtomicInteger nextSequenceNumber = new AtomicInteger(1);
	
	public AbstractScionServer(IPath serverExecutable,Writer serverOutput,File directory) {
		this.serverExecutable = serverExecutable;
		this.serverOutput=serverOutput;
		this.directory=directory;
	}
	
	protected int makeSequenceNumber() {
		return nextSequenceNumber.getAndIncrement();
	}
	
	/**
	 * Reads a line from the server's stdout (or stderr).
	 * Blocks until a line is available.
	 * The returned line does not contain the newline character(s) at the end.
	 * Returns null in case of EOF.
	 */
	protected String readLineFromServer() throws IOException {
		final String line = serverStdOutReader.readLine();
		if (line != null) {
			if (serverOutput!=null){
//				serverOutput.append(line+"\n");
//				int ix=0;
//				while (line.length()-ix>1024){
//					serverOutput.append(line.substring(ix,ix+1024));
//					//serverOutput.flush();
//					ix+=1024;
//				}
//				serverOutput.append(line.substring(ix,line.length())+"\n");
				// this seems to be what works the best to write to the console
				// async in display, cut in chunks so it's not too big
				try {
					Display theDisplay = Display.getDefault();
					theDisplay.asyncExec(new Runnable() {
						public void run() {
							try {
								//serverOutput.append(line);
								//serverOutput.append("\n");
								//serverOutput.flush();
								int ix=0;
								while (line.length()-ix>1024){
									serverOutput.append(line.substring(ix,ix+1024));
									//serverOutput.flush();
									ix+=1024;
								}
								serverOutput.append(line.substring(ix,line.length())+"\n");
								serverOutput.flush();
							} catch (IOException ioe){
								// ignore
							}
						}
					});
				} catch (SWTException e) {
					if (e.code == SWT.ERROR_THREAD_INVALID_ACCESS) {
					  // UI thread is actually dead at this point, so ignore
					  // (Note: This is actually semi-important on Mac OS X Cocoa.)
					} else {
					  // Re-throw the exception...
					  throw e;
					}
				}
			} else {
				Trace.trace(SERVER_STDOUT_PREFIX, line);
			}
		} else {
			if (serverOutput!=null){
				serverOutput.append("Server gave EOF on stdout\n");
				serverOutput.flush();
			} else {
				Trace.trace(CLASS_PREFIX, "Server gave EOF on stdout");
			}
		}
		return line;
	}
}
