package net.sf.eclipsefp.haskell.scion.client;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.ConnectException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.scion.commands.CommandStatus;
import net.sf.eclipsefp.haskell.scion.commands.ScionCommand;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

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
	private static final String
		THREAD_PREFIX = "[ScionClientThread]",
		SERVER_STDOUT_PREFIX = "[scion_server]",
		TO_SERVER_PREFIX = "[scion_server] <<",
		FROM_SERVER_PREFIX = "[scion_server] >>";

	private Process process;
	private BufferedReader serverStdOutReader;
	
	private Socket socket;
	private BufferedWriter socketWriter;
	private InputStream socketInputStream;
	
	private int port = -1; // negative if not yet captured from stdout
	private volatile boolean stopped = false; // set to true if we are to stop at the soonest possible occasion
	
	private CommandQueue commandQueue;

	public ScionClientThread(CommandQueue commandQueue) {
		super("Scion client thread");
		this.commandQueue = commandQueue;
		setUncaughtExceptionHandler(this);
	}
	
	/**
	 * Sits in a loop waiting for commands to arrive in the queue.
	 * Runs commands when they arrive, and <code>notify()</code>s the command object after completion.
	 * Exits its loop when <code>stopped</code> is set to <code>true</code>,
	 * which is done through calling the <code>stop()</code> method.
	 */
	@Override
	public void run() {
		Trace.trace(THREAD_PREFIX, "Started");
		try {
			// Start the server
			startServerProcess();
			capturePortNumber();
			connectToServer();
			startOutputSlurper();
			
			// Loop waiting for commands and running them
			int nextSequenceNumber = 1;
			while (!stopped) {
				checkRunning();
				
				ScionCommand command = null;
				Trace.trace(THREAD_PREFIX, "Waiting for commands");
				try {
					command = commandQueue.take(); // blocks until command is available
				} catch (InterruptedException ex) {
					// just continue
				}
				Trace.trace(THREAD_PREFIX, "Woken up");
				
				// If the command is already in a finished state (e.g. timeout),
				// we are picking up where a previous server incarnation crashed.
				// Don't bother to run the command again.
				if (!stopped && !command.getStatus().isFinished()) {
					command.setSequenceNumber(nextSequenceNumber);
					++nextSequenceNumber;
					syncRunCommand(command);
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
		String executable = ScionPlugin.getDefault().getPreferenceStore().getString(IPreferenceConstants.SCION_SERVER_EXECUTABLE);
		List<String> command = new LinkedList<String>();
		command.add(executable);
		command.add("--autoport");
		
		// Launch the process
		ProcessBuilder builder = new ProcessBuilder(command);
		builder.redirectErrorStream(true); // send server's stderr to its stdout
		process = builder.start();
		
		if (process == null) {
			throw new ScionClientException("Scion server process could not be started");
		}
		
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
	 * If not, it throws a {@link ScionClientException}.
	 */
	private void checkRunning() {
		if (process == null)
			throw new ScionClientException(String.format("Scion server did not start"));
		// There is no way to ask a Process directly whether it is still running,
		// so this is the best we can do...
		int exitValue;
		try {
			exitValue = process.exitValue();
		} catch (IllegalThreadStateException ex) {
			// Still running
			return;
		}
		throw new ScionClientException(String.format("Scion server has died with exit value %d", exitValue));
	}
	
	/**
	 * Reads from the server's stdout until EOF is reached.
	 * Useful to collect information after the server process has unexpectedly died.
	 * @return  
	 */
	private String lastWords() {
		// Collect a post-mortem by reading all that's left in the server's output stream
		if (serverStdOutReader == null)
			return "";
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
			line = readLineFromServer();
		} while (!line.startsWith(start));
		
		// Parse the port number from the string
		port = Integer.parseInt(line.substring(start.length()));
	}
	
	private void connectToServer() throws IOException {
		// Open the connection
		socket = connectToServer(host, port);	
		socketInputStream = socket.getInputStream();
		socketWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
	}
	
	/**
	 * Starts a thread that reads from the server's stdout (and stderr)
	 * and sends its output to the tracer (if tracing).
	 */
	private void startOutputSlurper() {
		new Thread() {
			@Override
			public void run() {
				String line;
				do {
					line = readLineFromServer();
				} while (line != null);
			}
		}.start();
	}
	
	/**
	 * Reads a line from the server's stdout (or stderr).
	 * Blocks until a line is available.
	 * Returns null in case of EOF or IOException.
	 */
	private String readLineFromServer() {
		String line;
		try {
			line = serverStdOutReader.readLine();
		} catch (IOException ex) {
			Trace.trace(THREAD_PREFIX, ex);
			return null;
		}
		if (line != null) {
			Trace.trace(SERVER_STDOUT_PREFIX, line);
		} else {
			Trace.trace(THREAD_PREFIX, "Server gave EOF on stdout");
		}
		return line;
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
	
	// Command handling -------------------------------------------------------
	
	/**
	 * Sends the command to the Scion server, and blocks waiting for it to return.
	 */
	private void syncRunCommand(ScionCommand command) throws IOException {
		Socket socket = null;
		try {
			sendCommand(command, socketWriter);
			command.setStatus(CommandStatus.RUNNING);
			
			// Receive the response
			Reader reader = new InputStreamReader(socketInputStream);
			JSONObject response = receiveResponse(reader);
			command.processResponse(response);
			command.setStatus(CommandStatus.SUCCESS);
			Trace.trace(THREAD_PREFIX, "Command executed successfully");
		} catch (IOException ex) {
			command.setStatus(CommandStatus.FAILED);
			throw ex;
		} catch (ScionClientException ex) {
			command.setStatus(CommandStatus.FAILED);
		} finally {
			// Always close the socket
			if (socket != null) {
				try {
					socket.close();
				} catch (IOException e) {
					// ignore, we were closing anyway
				}
			}
			// Wake up the sender of the command
			synchronized (command) {
				command.notifyAll();
			}
		}
	}
	
	private void sendCommand(ScionCommand command, Writer out) throws IOException {
		String jsonString = command.toString();
		
		Trace.trace(TO_SERVER_PREFIX, "%s", jsonString);

		out.write(jsonString);
		out.write("\n");
		out.flush();
	}
	
	private JSONObject receiveResponse(Reader reader) throws IOException {
		JSONObject response;
		try {
			response = new JSONObject(new JSONTokener(reader));
		} catch (JSONException ex) {
			throw new ScionClientException(String.format("Unable to parse Scion response"));
		}
		Trace.trace(FROM_SERVER_PREFIX, "%s", response.toString());
		return response;
	}

}
