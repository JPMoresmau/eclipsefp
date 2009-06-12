package net.sf.eclipsefp.haskell.scion.client;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.Thread.UncaughtExceptionHandler;
import java.lang.management.ThreadInfo;
import java.net.ConnectException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.commands.CommandStatus;
import net.sf.eclipsefp.haskell.scion.commands.ScionCommand;

/**
 * A thread that acts as a client to the running Scion server process.
 * Its lifetime is coupled to the server's lifetime:
 * the thread stops running when the server dies or exits,
 * and if the thread is stopped, the server process is also killed.
 * 
 * This is a separate thread because the server can only process one command at a time.
 * In this way, we model the same behaviour on the client side.
 * Using a thread also allows for asynchronous commands.
 * 
 * The thread uses a dummy object as a monitor; the thread waits on it and is notified
 * when a new command arrives in the queue. (Note that we have to own the monitor of the
 * object we are waiting on, i.e. be synchronized on that object. So we cannot wait on the
 * thread object itself, because then nobody could call into it to add commands; nor can we
 * wait on the queue, because then no commands could be placed into it.)
 * 
 * @author Thomas ten Cate
 */
public class ScionClientThread extends Thread implements UncaughtExceptionHandler {

	private static final String host = null; // amounts to connecting to the loopback interface
	private static final int MAX_RETRIES = 5;
	private static final String ENCODING = "UTF-8";
	private static final String
		THREAD_PREFIX = "[ScionClientThread]",
		SERVER_STDOUT_PREFIX = "[scion_server]",
		TO_SERVER_PREFIX = "[scion_server] <<",
		FROM_SERVER_PREFIX = "[scion_server] >>";

	private Process process;
	private BufferedReader serverStdOutReader;
	private int port = -1; // negative if not yet captured from stdout
	private volatile boolean stopped = false; // set to true if we are to stop at the soonest possible occasion
	
	private CommandQueue commandQueue;

	public ScionClientThread(CommandQueue commandQueue) {
		super("Scion server thread");
		this.commandQueue = commandQueue;
		setUncaughtExceptionHandler(this);
	}
	
	/**
	 * Sits in a loop waiting for commands to arrive in the queue.
	 * Runs commands when they arrive, and <code>notify()</code>s the command object after completion.
	 * Exits its loop when <code>stopped</code> is set to <code>true</code>,
	 * which is done through calling the <code>stop()</code> method.
	 */
	public void run() {
		Trace.trace(THREAD_PREFIX, "Started");
		try {
			// Start the server
			startServerProcess();
			
			// Loop waiting for commands and running them
			int nextSequenceNumber = 1;
			while (!stopped) {
				checkRunning();
				
				ScionCommand command = null;
				if (port < 0) {
					capturePortNumber();
				}
				
				Trace.trace(THREAD_PREFIX, "Waiting for commands");
				try {
					command = commandQueue.take(); // blocks until command is available
				} catch (InterruptedException ex) {
					// just continue
				}
				Trace.trace(THREAD_PREFIX, "Woken up");
				
				if (!stopped && command != null) {
					command.setSequenceNumber(nextSequenceNumber);
					++nextSequenceNumber;
					syncRunCommand(command);
					port = -1;
				}
			}
			Trace.trace(THREAD_PREFIX, "Received stop notification");
		} catch (Exception ex) {
			Trace.trace(THREAD_PREFIX, ex);
			String lastWords = lastWords();
			Trace.trace(THREAD_PREFIX, "The server's last words were:\n%s", lastWords);
		} finally {
			// Stop the server
			stopServerProcess();
			Trace.trace(THREAD_PREFIX, "Ended");
		}
	}
	
	public void die() {
		Trace.trace(THREAD_PREFIX, "Dying");
		stopped = true;
		interrupt();
	}
	
	public void uncaughtException(Thread thread, Throwable ex) {
		Trace.trace(THREAD_PREFIX, ex);
	}
	
	// Server process handling ------------------------------------------------

	/**
	 * Launches the external Scion server process.
	 * 
	 * Returns immediately if the process has been launched successfully;
	 * throws an exception otherwise.
	 */
  	private void startServerProcess() throws IOException {
  		Trace.trace(THREAD_PREFIX, "Starting server");
  		
  		// Construct the command line
		String executable = "/home/thomas/.cabal/bin/scion_server"; // TODO autodetect, make configurable
		List<String> command = new LinkedList<String>();
		command.add(executable);
		
		// Launch the process
		ProcessBuilder builder = new ProcessBuilder(command);
		builder.redirectErrorStream(true); // send server's stderr to its stdout
		process = builder.start();
		
		// Connect to the process's stdout to capture messages
		// Assume that status messages and such will be ASCII only 
		serverStdOutReader = new BufferedReader(new InputStreamReader(process.getInputStream(), "US-ASCII"));
		
		Trace.trace(THREAD_PREFIX, "Server started");
	}
  	
	/**
	 * Stops the server (if running) and frees up resources.
	 * It is allowed to call this method in any state.
	 * After this call, <code>alive</code> is guaranteed to be false.
	 */
	private void stopServerProcess() {
		Trace.trace(THREAD_PREFIX, "Stopping server");
		
		try {
			if (serverStdOutReader != null) {
				serverStdOutReader.close();
				serverStdOutReader = null;
			}
		} catch (IOException ex) {
			// ignore, we wanted to close it anyway
		}
		if (process != null) {
			process.destroy();
			process = null;
		}
		
		Trace.trace(THREAD_PREFIX, "Server stopped");
	}
	
	/**
	 * Attempts to check whether the server process is still running.
	 * If not, it throws a {@link ScionServerException}.
	 */
	private void checkRunning() {
		// There is no way to ask a Process directly whether it is still running,
		// so this is the best we can do...
		int exitValue;
		try {
			exitValue = process.exitValue();
		} catch (IllegalThreadStateException ex) {
			// Still running
			return;
		}
		throw new ScionServerException(String.format("Scion server has died with exit value %d", exitValue));
	}
	
	/**
	 * Reads from the server's stdout until EOF is reached.
	 * Useful to collect information after the server process has unexpectedly died.
	 * @return  
	 */
	private String lastWords() {
		// Collect a post-mortem by reading all that's left in the server's output stream
		StringBuffer lastWords = new StringBuffer();
		String line;
		do {
			try {
				line = serverStdOutReader.readLine();
			} catch (IOException e) {
				line = null;
			}
			if (line != null) {
				lastWords.append(line);
				lastWords.append("\n");
			}
		} while (line != null);
		return lastWords.toString();
	}

	// Server communication ---------------------------------------------------
	
  	/**
  	 * Reads from the server process's stdout until the port number is printed,
  	 * then stores it in <code>port</code>.
  	 * This blocks until the port number is read, or throws an exception.
  	 */
	private void capturePortNumber() throws IOException {
		// Wait until the port number is printed
		final String start = "=== Listening on port: ";
		String line;
		do {
			line = serverStdOutReader.readLine();
			if (line == null) {
				throw new ScionServerException("Server process gave EOF before printing port number");
			}
			Trace.trace(SERVER_STDOUT_PREFIX, line);
		} while (!line.startsWith(start));
		
		// Parse the port number from the string
		port = Integer.parseInt(line.substring(start.length()));
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
	
	/**
	 * Reads exactly <code>length</code> bytes from the input stream.
	 * Blocks until they are available. Then, decodes them and returns them in a <code>String</code>.
	 * (Note that, due to encoding issues, the resulting string may be of a different length
	 * than the number of bytes read.) 
	 */
	private String read(InputStream in, int length) throws IOException {
		byte[] buffer = new byte[length];
		int count = 0;
		while (count < length) {
			int read = in.read(buffer, count, length - count);
			if (read < 0) {
				throw new ScionServerException(String.format("Unexpected end-of-file when reading %d bytes from server", length));
			}
			count += read;
		}
		return new String(buffer, ENCODING);
	}
	
	// Command handling -------------------------------------------------------
	
	/**
	 * Sends the command to the Scion server, and blocks waiting for it to return.
	 */
	private void syncRunCommand(ScionCommand command) throws IOException {
		Socket socket = null;
		try {
			// Open the connection
			socket = connectToServer(host, port);

			// Send the command
			BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
			sendCommand(command, writer);
			command.setStatus(CommandStatus.RUNNING);
			
			// Receive the response
			InputStream in = socket.getInputStream();
			String responseLisp = receiveResponse(in);
			try {
				command.receiveResponse(responseLisp);
				command.setStatus(CommandStatus.SUCCESS);
			} catch (ScionParseException ex) {
				command.setStatus(CommandStatus.FAILED);
				Trace.trace(THREAD_PREFIX, ex);
			}
		} catch (IOException ex) {
			command.setStatus(CommandStatus.FAILED);
			throw ex;
		} finally {
			// Always close the socket
			if (socket != null) {
				try {
					socket.close();
				} catch (IOException e) {
					// ignore, we were closing anyway
				}
			}
		}
	}
	
	private void sendCommand(ScionCommand command, Writer out) throws IOException {
		String lisp = command.toLisp();
		String length = encodeLength(lisp.length() + 1);
		
		Trace.trace(TO_SERVER_PREFIX, "%s", lisp);

		out.write(length);
		out.write(lisp);
		out.write("\n");
		out.flush();
	}
	
	private String receiveResponse(InputStream in) throws IOException {
		// First, 6 hex characters encoding the length of the rest of the message
		String lengthHex = read(in, 6);
		int length = decodeLength(lengthHex);
		
		// Then the rest of the message
		String response = read(in, length);
		
		Trace.trace(FROM_SERVER_PREFIX, "%s", response);

		return response;
	}
	
	private String encodeLength(int length) {
		return String.format("%06x", length);
	}
	
	private int decodeLength(String length) {
		return Integer.parseInt(length, 16);
	}

}
