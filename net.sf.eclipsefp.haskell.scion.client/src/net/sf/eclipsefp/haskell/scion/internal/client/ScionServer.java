package net.sf.eclipsefp.haskell.scion.internal.client;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.ConnectException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.LinkedList;
import java.util.List;

import javax.print.attribute.standard.Severity;

import org.eclipse.core.runtime.IProgressMonitor;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerConnectException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;

/**
 * Representation of the Scion server on the Java side.
 * 
 * @author Thomas ten Cate
 */
public class ScionServer {

	private static final String host = null; // amounts to connecting to the loopback interface
	private static final int MAX_RETRIES = 5;
	private static final String
		CLASS_PREFIX = "[ScionServer]",
		SERVER_STDOUT_PREFIX = "[scion-server]";
	
	private String serverExecutable;
	
	private Process process;
	private BufferedReader serverStdOutReader;
	private Socket socket;
	private BufferedReader socketReader;
	private BufferedWriter socketWriter;
	
	private int port = -1; // negative if not yet captured from stdout
	
	private int nextSequenceNumber = 1;

	private Thread serverOutputThread;
	
	public ScionServer(String serverExecutable) {
		this.serverExecutable = serverExecutable;
	}
	
	/**
	 * Starts the Scion server.
	 */
	public synchronized void startServer() throws ScionServerStartupException {
		startServerProcess();
		capturePortNumber();
		connectToServer();
		serverOutputThread=new Thread(){
			public void run(){
				while (serverStdOutReader!=null){
					slurpServerOutput2();
					try {
						Thread.sleep(100);
					} catch (InterruptedException ie){
						// noop
					}
				}
			}
		};
		serverOutputThread.setDaemon(true);
		serverOutputThread.start();
		//slurpServerOutput();
	}
	
	/**
	 * Stops the Scion server under all circumstances.
	 */
	public synchronized void stopServer() {
		/*try {
			slurpServerOutput();
		} catch (Throwable ex) {
			// ignore
		}*/
		stopServerProcess();
	}
	
	////////////////////////////
	// Server process handling

	/**
	 * Launches the external Scion server process.
	 * 
	 * Returns immediately if the process has been launched successfully;
	 * throws an exception otherwise.
	 * 
	 * @throws ScionServerStartupException if the server could not be started;
	 *         the inner exception will give detailed information 
	 */
  	private void startServerProcess() throws ScionServerStartupException {
  		Trace.trace(CLASS_PREFIX, "Starting server");
  		
  		// Construct the command line
		String executable = serverExecutable;
		List<String> command = new LinkedList<String>();
		command.add(executable);
		command.add("--autoport");
		
		// Launch the process
		ProcessBuilder builder = new ProcessBuilder(command);
		builder.redirectErrorStream(true); // send server's stderr to its stdout
		try {
			process = builder.start();
		} catch (Throwable ex) {
			throw new ScionServerStartupException(UITexts.scionServerCouldNotStart_message, ex);
		}
		
		// Connect to the process's stdout to capture messages
		// Assume that status messages and such will be ASCII only
		try {
			serverStdOutReader = new BufferedReader(new InputStreamReader(process.getInputStream(), "US-ASCII"));
		} catch (UnsupportedEncodingException ex) {
			// make compiler happy, because US-ASCII is always supported
		}
		Trace.trace(CLASS_PREFIX, "Server started");
	}
  	
	/**
	 * Stops the server (if running) and frees up resources.
	 * It is allowed to call this method in any state.
	 */
	private void stopServerProcess() {
		Trace.trace(CLASS_PREFIX, "Stopping server");
		
		if (serverStdOutReader != null) {
			try {
				serverStdOutReader.close();
				serverStdOutReader = null;
			} catch (Throwable ex) {
				// ignore
			}
		}
		if (socketWriter != null) {
			try {
				socketWriter.close();
				socketWriter = null;
			} catch (Throwable ex) {
				// ignore
			}
		}
		if (socketReader != null) {
			try {
				socketReader.close();
				socketReader = null;
			} catch (Throwable ex) {
				// ignore
			}
		}
		if (socket != null) {
			try {
				socket.close();
				socket = null;
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
	
	/**
	 * Reads from the server's stdout until EOF is reached.
	 * Useful to collect information after the server process has unexpectedly died.
	 * 
	 * Handles all exceptions internally.
	 * 
	 * @return all data readily available on the server's stdout.
	 * Returns <code>null</code> if there is no stdout to read from.
	 */
	private String lastWords() {
		// Collect a post-mortem by reading all that's left in the server's output stream
		if (serverStdOutReader == null)
			return null;
		StringBuffer lastWords = new StringBuffer();
		try {
			while (serverHasOutput()) {
				String line = readLineFromServer();
				if (line == null) {
					break;
				}
				if (lastWords.length() > 0) {
					lastWords.append("\n");
				}
				lastWords.append(line);
			}
		} catch (IOException ex) {
			// ignore
		}
		return lastWords.toString().trim();
	}
	
	////////////////////////////////
	// Server stdout communication
	
  	/**
  	 * Reads from the server process's stdout until the port number is printed,
  	 * then stores it in <code>port</code>.
  	 * This blocks until the port number is read, or throws an exception.
  	 * 
  	 * @throws ScionServerConnectException if the port number could not be read for some reason 
  	 */
	private void capturePortNumber() throws ScionServerConnectException {
		try {
			// Wait until the port number is printed
			final String start = "=== Listening on port: ";
			String line;
			do {
				line = readLineFromServer();
				if (line == null) {
					throw new ScionServerConnectException(UITexts.scionServerOutputReadError_message);
				}
			} while (!line.startsWith(start));
			
			// Parse the port number from the string
			port = Integer.parseInt(line.substring(start.length()));
		} catch (IOException ex) {
			throw new ScionServerConnectException(UITexts.scionServerConnectError_message, ex);
		}
	}	

	/**
	 * Reads from the server's stdout (and stderr)
	 * and sends its output to the tracer (if tracing).
	 * If there is no output ready to be read, this returns immediately.
	 * 
	 * Errors while reading are silently ignored.
	 */
	private void slurpServerOutput2() {
		try {
			while (serverHasOutput()) {
				String line = readLineFromServer();
				if (line == null) {
					break;
				}
			}
		} catch (IOException ex) {
			// too bad, ignore
		}
	}
	
	private boolean serverHasOutput() throws IOException {
		return serverStdOutReader.ready();
	}
	
	/**
	 * Reads a line from the server's stdout (or stderr).
	 * Blocks until a line is available.
	 * The returned line does not contain the newline character(s) at the end.
	 * Returns null in case of EOF.
	 */
	private String readLineFromServer() throws IOException {
		String line = serverStdOutReader.readLine();
		if (line != null) {
			Trace.trace(SERVER_STDOUT_PREFIX, line);
		} else {
			Trace.trace(CLASS_PREFIX, "Server gave EOF on stdout");
		}
		return line;
	}
	
	////////////////////////////////
	// Server socket communication
	
	private void connectToServer() throws ScionServerConnectException {
		try {
			socket = connectToServer(host, port);	
			socketReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
			socketWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
		} catch (Throwable ex) {
			throw new ScionServerConnectException(UITexts.scionServerConnectError_message, ex);
		}
	}
	
	/**
	 * Attempts to connect to the open TCP socket of the Scion server process.
	 * 
	 * Returns as soon as a successful connection is made. If a connection cannot
	 * be established within a second or so, an exception is thrown.
	 */
	private Socket connectToServer(String host, int port) throws UnknownHostException, IOException {
		int count = 0;
		Socket socket = null;
		
		do {
			// Try making the connection
			try {
				socket = new Socket(host, port);
			} catch (ConnectException ex) {
				++count;
				if (count == MAX_RETRIES) {
					// We've tried hard enough now, throw it out
					throw ex;
				}
			}
	
			// Don't busy-wait, use incremental fallback
			try {
				Thread.sleep(128 * count);
			} catch (InterruptedException ex) {
				// that's okay, ignore
			}
		} while (socket == null);
		
		return socket;
	}

  	/////////////////////
	// Command handling
	
	/**
	 * Sends the command to the Scion server, and blocks waiting for it to return.
	 * 
	 * Note that this is being called from another thread
	 * (in particular, some thread from the Eclipse job scheduler).
	 * Even though the scheduler should ensure that only one command is running
	 * at any time, we make it synchronized to be sure.
	 * 
	 * @throws ScionServerException if something happened to the server connection
	 * @throws ScionCommandException if something went wrong parsing or processing the command 
	 */
	public synchronized void runCommandSync(ScionCommand command,IProgressMonitor monitor) throws ScionServerException, ScionCommandException {
		if (process == null || socketReader == null || socketWriter == null) {
			throw new ScionServerException(UITexts.scionServerNotRunning_message);
		}
		try {
			command.setSequenceNumber(makeSequenceNumber());
			command.sendCommand(socketWriter,monitor);
			serverOutputThread.interrupt();
			//slurpServerOutput();
			command.receiveResponse(socketReader,monitor);
			//slurpServerOutput();
			Trace.trace(CLASS_PREFIX, "Command executed successfully");
		} catch (ScionServerException ex) {
			ex.setLastWords(lastWords());
			throw ex;
		}
	}
	
	private int makeSequenceNumber() {
		return nextSequenceNumber++;
	}

}
