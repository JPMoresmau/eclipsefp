package net.sf.eclipsefp.haskell.scion.internal.servers;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.jobs.ISchedulingRule;


/**
 * Scion-server command runner interface
 * 
 * @author JP Moresmau and others.
 */
public interface IScionCommandRunner extends ISchedulingRule {
  /**
   * Get the project associated with this command
   * 
   * @return the project associated with this command, or null, if no project is
   *         associated.
   */
  public IProject getProject();
}
