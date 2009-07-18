// Copyright (c) 2009 by Thomas ten Cate <ttencate@gmail.com>
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.scion.internal.client;

import java.io.IOException;
import java.lang.Thread.State;

import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;

import org.eclipse.core.runtime.jobs.ISchedulingRule;

/**
 * Manages an instance of the Scion server thread.
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
public class ScionThreadManager implements ISchedulingRule {
	
	private String serverExecutable;
	private ScionClientThread client;
	
	private static final String SCION_PREFIX = "[Scion]";
	
	/**
	 * Starts a new Scion instance.
	 * 
	 * @param serverExecutable the absolute path to the scion_server executable
	 */
	public ScionThreadManager(String serverExecutable) {
		this.serverExecutable = serverExecutable;
		tryEnsureInstance();
	}
	
	///////////////////////////
	// Server thread handling
	
	/**
	 * Does its best to ensure that a server is ready to receive our commands
	 * upon return of this method. However, this does not have to be the case.
	 * 
	 * This call is guaranteed to succeed without exceptions.
	 * At exit, <code>server</code> is guaranteed to be not <code>null</code>.
	 */
	private void tryEnsureInstance() {
		if (client == null) {
			Trace.trace(SCION_PREFIX, "No server thread exists yet; creating new thread");
			createClient();
		} else if (client.getState() == State.TERMINATED) {
			Trace.trace(SCION_PREFIX, "Existing server thread exited; creating new thread");
			createClient();
		}
	}
	
	/**
	 * Unconditionally creates a new server thread.
	 * 
	 * Guaranteed to succeed without exceptions.
	 */
	private void createClient() {
		client = new ScionClientThread(serverExecutable);
		client.start();
		Thread.yield(); // give the client a chance to start
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
	public void dispose() {
		if (client != null) {
			Trace.trace(SCION_PREFIX, "Killing the client thread");
			client.die();
			client = null;
		}
	}
	
	/////////////////////
	// Command handling
	
	public int makeSequenceNumber() {
		return client.makeSequenceNumber();
	}
	
	/**
	 * Runs the command on the server. Returns when the command is done, or an error occurred.
	 */
	public void runCommandSync(ScionCommand command) throws IOException {
		client.syncRunCommand(command);
	}
	
	/////////////////////////////////
	// methods from ISchedulingRule

	public boolean contains(ISchedulingRule rule) {
		return rule == this;
	}

	public boolean isConflicting(ISchedulingRule rule) {
		return rule == this;
	}
	
}
