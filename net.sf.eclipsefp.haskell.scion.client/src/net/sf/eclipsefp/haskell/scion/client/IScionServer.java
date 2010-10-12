package net.sf.eclipsefp.haskell.scion.client;

import org.eclipse.core.runtime.IProgressMonitor;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

/**
 * interface for communication with scion
 * @author JP Moresmau
 *
 */
public interface IScionServer {
	
	/**
	 * start the server process and connect to it
	 * @throws ScionServerStartupException
	 */
	void startServer() throws ScionServerStartupException;
	
	/**
	 * stop the server
	 */
	void stopServer();
	
	/**
	 * run the command synchronously
	 * @param command the command
	 * @param monitor the monitor to manage cancellation, etc.
	 * @throws ScionServerException
	 * @throws ScionCommandException
	 */
	void runCommandSync(ScionCommand command,IProgressMonitor monitor) throws ScionServerException, ScionCommandException;
	
	/**
	 * Check server's protocol version, log a message if it's not a match.
	 */
	void checkProtocol(IScionCommandRunner cmdRunner);
}
