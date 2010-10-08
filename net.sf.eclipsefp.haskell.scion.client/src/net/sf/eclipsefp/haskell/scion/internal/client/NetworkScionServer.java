package net.sf.eclipsefp.haskell.scion.internal.client;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Representation of the Scion server on the Java side.
 * 
 * @author Thomas ten Cate
 */
public class NetworkScionServer extends AbstractScionServer {
	/** Number of times to try to connect or relaunch an operation on timeout */
	private static final int MAX_RETRIES = 5;
	/** Accept thread initial timeout, 1/2 second in milliseconds */
	private static final int ACCEPT_INITIAL_TMO = 1000 / 2;
	
	private static final AtomicInteger threadNb = new AtomicInteger(1);
	private static final AtomicInteger acceptNb = new AtomicInteger(1);
	
	private Socket socket;
	private BufferedReader socketReader;
	private BufferedWriter socketWriter;
	
	private Thread serverOutputThread;
	
	public NetworkScionServer(IPath serverExecutable,Writer serverOutput,File directory) {
		super(serverExecutable,serverOutput,directory);
	}
	
	/**
	 * Starts the Scion server.
	 */
	public synchronized void startServer() throws ScionServerStartupException {
		startServerProcess();
		// connectToServer(port);
		serverOutputThread=new Thread(CLASS_PREFIX + (threadNb.getAndIncrement())){
			public void run(){
				while (serverStdOutReader!=null){
					slurpServerOutput();
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
  		try {
  		  final ServerSocket bindSock = new ServerSocket();
  		  bindSock.bind(null, 1);
  		  ScionPlugin.logInfo("== bindSock = ".concat(bindSock.getLocalSocketAddress().toString()));
  		  
  		  AcceptThread acceptJob = new AcceptThread("scionServer-accept" + acceptNb.getAndIncrement(), this, bindSock);
  		  
  		  acceptJob.setDaemon(true);
  		  acceptJob.start();
    		
	   	  // Construct the command line
    	  List<String> command = new LinkedList<String>();
    	  command.add(serverExecutable.toOSString());
    	  command.add("-c");
    	  command.add("-p");
    	  command.add(String.valueOf(bindSock.getLocalPort()));
    	  
    	  // Launch the process
    	  ProcessBuilder builder = new ProcessBuilder(command);
    	  if (directory!=null && directory.exists()) {
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
    	  Trace.trace(CLASS_PREFIX, "Server launched.");
    	  
		  int retries = 0;
    	  synchronized (this) {
    		  int tmo = ACCEPT_INITIAL_TMO;

    		  while (   retries < MAX_RETRIES
    				  && socket == null) {
    			  try {
    				  this.wait(tmo);
    				  tmo *= 2;
    				  retries += 1;
    			  } catch (InterruptedException e) {
    				  // Ignore the interruption and continue.
    			  }
    		  }
    	  }
		  
		  if (retries >= MAX_RETRIES || socket == null) {
			  ScionPlugin.logInfo("== Accept thread did not receive connection, terminating.");
			  acceptJob.setTerminate();
			  try {
				  acceptJob.join();
				  ScionPlugin.logInfo("== Accept thread terminated.");
				  throw new ScionServerStartupException(UITexts.scionServerCouldNotStart_message);
			  } catch (InterruptedException e) {
				  // Don't care...
			  }
		  }

		  socketReader = new BufferedReader(new InputStreamReader(socket.getInputStream(),"UTF8"));
		  socketWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream(),"UTF8"));
		  
		  // Don't need the bind socket any more...
		  bindSock.close();
  		} catch (IOException e) {
  			// Should not fail, since we're asking for an ephemeral port, but report it
  			// nonetheless
  			throw new ScionServerStartupException(UITexts.scionServerCouldNotStart_message, e);
  		}
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
	 * Reads from the server's stdout (and stderr)
	 * and sends its output to the tracer (if tracing).
	 * If there is no output ready to be read, this returns immediately.
	 * 
	 * Errors while reading are silently ignored.
	 */
	private void slurpServerOutput() {
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
		return (   serverStdOutReader != null
				&& serverStdOutReader.ready());
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
	public void runCommandSync(ScionCommand command,IProgressMonitor monitor) throws ScionServerException, ScionCommandException {
		// set only once
		command.setSequenceNumber(makeSequenceNumber());
		runCommandSync(command, monitor,0);
	}
	
	private void runCommandSync(ScionCommand command,IProgressMonitor monitor,int count) throws ScionServerException, ScionCommandException {
		if (process == null || socketReader == null || socketWriter == null) {
			throw new ScionServerException(UITexts.scionServerNotRunning_message);
		}
		try {
			command.sendCommand(socketWriter,monitor);
			serverOutputThread.interrupt();
			//slurpServerOutput();
			command.receiveResponse(socketReader,monitor);
			serverOutputThread.interrupt();
			//slurpServerOutput();
			//Trace.trace(CLASS_PREFIX, "Command executed successfully");
		} catch (ScionServerException ex) {
			if (count<MAX_RETRIES && ex.getCause()!=null && ex.getCause().getCause() instanceof SocketTimeoutException){
				// this will not change sequence number
				runCommandSync(command,monitor,count+1);
			} else {
				ex.setLastWords(lastWords());
				throw ex;
			}
		}
	}
	
	private class AcceptThread extends Thread {
		private NetworkScionServer instance;
		private ServerSocket bindSock;
		private boolean terminateFlag;
		
		public AcceptThread(String name, NetworkScionServer instance, ServerSocket bindSock) {
			super(name);
			this.instance = instance;
			this.bindSock = bindSock;
			this.terminateFlag = false;
		}
		
		public void run() {
			int timeout = 1000 / 2;
			try {
				bindSock.setSoTimeout(timeout);
				while (!terminateFlag) {
					socket = bindSock.accept();
					synchronized (instance) {
						instance.notifyAll();
					}
				} 
			} catch (SocketException e) {
				// Yeah, yeah, accept timed out...
			} catch (IOException e) {
				// Retry accepting connections...
			} 
		}
		
		public void setTerminate() {
			terminateFlag = true;
		}
	}
}
