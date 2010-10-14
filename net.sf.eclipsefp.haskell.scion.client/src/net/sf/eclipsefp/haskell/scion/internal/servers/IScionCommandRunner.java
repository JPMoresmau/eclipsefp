package net.sf.eclipsefp.haskell.scion.internal.servers;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

/**
 * Scion-server command runner interface
 * 
 * @author JP Moresmau and others.
 */
public interface IScionCommandRunner extends ISchedulingRule {
  /**
   * Send a command to the scion-server.
   * 
   * @param command
   *          The scion command
   * @param monitor
   *          A progress monitor, if the command is long-running
   * 
   * @throws ScionServerException
   *           when communication with the scion-server is disrupted
   * @throws ScionCommandException
   *           when the command's response isn't what is expected.
   */
  public void sendCommand(ScionCommand command, IProgressMonitor monitor) throws ScionServerException, ScionCommandException;
  /**
   * Get the project associated with this command
   * 
   * @return the project associated with this command, or null, if no project is
   *         associated.
   */
  public IProject getProject();
}
