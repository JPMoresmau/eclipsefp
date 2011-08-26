/**
 * (c) 2010, B. Scott Michel
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.scion.client;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;

/**
 * A command group is a logical grouping of commands that excludes other commands from running
 * that operate on the same project or that project's resources.
 * 
 * @author B. Scott Michel (bscottm@ieee.org)
 */
public abstract class CommandGroup extends Job {
  /**
   * Initialize a command group. This also installs the scheduling rule for the Job.
   * rule.
   * 
   * @param jobName
   *          The Job's name.
   * @param theFile
   *          The command group's file.
   * @param projectRule
   *          If true, installs the file's project as part of the scheduling rule.
   */
  protected CommandGroup(String jobName, IFile theFile, boolean projectRule) {
    super(jobName);
    
    if (projectRule) {
      // Install a MultiRule for the project, not just for the file:
      setRule(new MultiRule ( new ISchedulingRule[] {
          theFile.getProject(),
          theFile
      } ) );
    } else {
      setRule( theFile );
    }
  }
  
  /**
   * Run the command group synchronously, expecting a result.
   */
  public IStatus runGroupSynchronously() {
    schedule();
    
    while (getResult() == null) {
      try {
        join();
      } catch (InterruptedException irq) {
        // Return something reasonable.
        return Status.CANCEL_STATUS;
      }
    }
    
    return getResult();
  }
}
