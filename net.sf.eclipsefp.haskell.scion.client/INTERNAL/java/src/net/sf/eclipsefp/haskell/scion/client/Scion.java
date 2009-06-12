// Copyright (c) 2009 by Thomas ten Cate <ttencate@gmail.com>
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.scion.client;

import java.lang.Thread.State;

import net.sf.eclipsefp.haskell.scion.commands.CommandStatus;
import net.sf.eclipsefp.haskell.scion.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.lisp.LispParser;
import net.sf.eclipsefp.haskell.scion.util.ThreadUtil;

/**
 * A static class that manages the external Scion server.
 *
 * This maintains the lifecycle of the server (initializes it and disposes
 * of it) and accepts Scion commands, either asynchronously or synchronously
 * (with optional timeout).
 *
 * This class provides a 'best-effort' connection to the server; it tries to
 * restart the server if it dies, etcetera. However, if things do not work out,
 * the caller will not receive an exception, but rather, its commands will fail.
 * 
 * TODO When the server cannot be run, all commands will be waiting to time out.
 * We should refuse them, or have them time out immediately, instead.
 *
 * @author Thomas ten Cate
 */
public class Scion {
	
	private static CommandQueue commandQueue = new CommandQueue();
	private static ScionClientThread client;
	
	private static String SCION_PREFIX = "[Scion]";
	
	/**
	 * Prevent instantiation of singleton class.
	 */
	private Scion() {
	}
	
	// Server thread handling -------------------------------------------------
	
	/**
	 * Pre-starts the server in anticipation of its use. This need not be called,
	 * but may result in faster response times for subsequent commands.
	 */
	public static void initializeClient() {
		tryEnsureInstance();
	}
	
	/**
	 * Does its best to ensure that a server is ready to receive our commands
	 * upon return of this method. However, this does not have to be the case.
	 * 
	 * This call is guaranteed to succeed without exceptions.
	 * At exit, <code>server</code> is guaranteed to be not <code>null</code>.
	 */
	private static void tryEnsureInstance() {
		if (client == null) {
			Trace.trace(SCION_PREFIX, "No server thread exists yet; creating new thread");
			createClient();
		} else if (client.getState() == State.TERMINATED) {
			Trace.trace(SCION_PREFIX, "Existing server thread exited; creating new thread");
			createClient();
		}
		// Ensure that the Lisp parser classes are loaded too
		LispParser.parse("()");
	}
	
	/**
	 * Unconditionally creates a new server thread.
	 * 
	 * Guaranteed to succeed without exceptions.
	 */
	private static void createClient() {
		client = new ScionClientThread(commandQueue);
		client.start();
		checkProtocol();
	}
	
	/**
	 * Stops the Scion server (if running) and disposes all resources.
	 * Does nothing if the server is not running. 
	 * 
	 * Subsequent calls to any of the server methods (except this <code>dispose</code> method)
	 * will result in an <code>IllegalStateException</code>.
	 * 
	 * @note This method is called by the framework and should not be called by clients.
	 */
	public static void dispose() {
		if (client != null) {
			Trace.trace(SCION_PREFIX, "Killing the client thread");
			client.die();
			client = null;
		}
	}
	
	// Command handling -------------------------------------------------------
	
	/**
	 * Enqueues the command, to be run on the server side.
	 * The <code>command</code> object functions as a monitor, which will be notified when the command is completed.
	 */
	public static void asyncRunCommand(ScionCommand command) {
		tryEnsureInstance();
		enqueueCommand(command);
	}
	
	/**
	 * Runs the command on the server, and waits for a limited time for it to complete.
	 * 
	 * @param timeout maximum time to wait in milliseconds
	 */
	public static void syncRunCommand(ScionCommand command, long timeout) {
		asyncRunCommand(command);
		synchronized (command) {
			ThreadUtil.waitTimeout(command, timeout);
			if (!command.getStatus().isFinished()) {
				Trace.trace(SCION_PREFIX, "Command in status %s timed out", command.getStatus().toString());
				command.setStatus(CommandStatus.TIMEOUT);
			} else {
				Trace.trace(SCION_PREFIX, "Command status: %s", command.getStatus().toString());
			}
		}
	}
	
	private static void enqueueCommand(ScionCommand command) {
		// Set to ENQUEUED first, before we even know that it's successful, to avoid race condition
		// where the client thread processes and finalizes the command before we even set it to ENQUEUED.
		command.setStatus(CommandStatus.ENQUEUED);
		if (commandQueue.offer(command)) {
			Trace.trace(SCION_PREFIX, "Command enqueued");
		} else {
			command.setStatus(CommandStatus.QUEUE_FULL);
			Trace.trace(SCION_PREFIX, "Command queue full; command not enqueued");
		}
	}
	
	// Specific commands ------------------------------------------------------
	
	private static void checkProtocol() {
		ConnectionInfoCommand command = new ConnectionInfoCommand();
		syncRunCommand(command, 2000);
		if (command.isSuccessful()) {
			// TODO
		}
	}
	
}
