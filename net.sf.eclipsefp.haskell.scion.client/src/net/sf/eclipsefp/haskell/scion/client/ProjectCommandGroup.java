/**
 * (c) 2010, B. Scott Michel
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.scion.client;



import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;

/**
 * A project command group is a logical grouping of commands that excludes other commands from running
 * that operate on the same project or that project's resources.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public abstract class ProjectCommandGroup extends Job {
  /** The project against which this command group holds exclusive access */
  IProject theProject;
  
  /**
   * The constructor used for project-based command groups.
   * 
   * @note This calls {@link ProjectCommandGroup#ProjectCommandGroup(String, IProject, boolean)} constructor
   * to install the scheduling rule.
   */
  
  public ProjectCommandGroup(String jobName, IProject theProject) {
    this(jobName, theProject, true);
  }
  
  /**
   * Initialize a project command group, optionally installing a scheduling
   * rule.
   * 
   * @param jobName
   *          The Job's name.
   * @param theProject
   *          The command group's project.
   * @param installRule
   *          If true, installs the project-based command group scheduling rule. Subclasses should
   *          pass false in order to install their own scheduling rules.
   */
  protected ProjectCommandGroup(String jobName, IProject theProject, boolean installRule) {
    super(jobName);
    this.theProject = theProject;
    if (installRule) {
      HaskellProjectSchedulingRule rule = new HaskellProjectSchedulingRule(theProject);
      
      setRule(rule);
      setName( adornedName(jobName, rule) );
    }
  }
  
  /**
   * Formats the Job's name, includes the command group's sequence number for easier
   * rule scheduling debugging (should see command groups partially ordered by sequence
   * number when executed.)
   * 
   * @param jobName The Job's name
   * @return "Job Name [seqno]" string.
   */
  protected String adornedName(final String jobName, CommandGroupSchedulingRule rule) {
    return jobName.concat(" [group ").concat(String.valueOf(rule.getCommandGroupSeqno())).concat("]");
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
        // Keep going...
      }
    }
    
    return getResult();
  }
}
