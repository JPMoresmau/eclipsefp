/**
 * (c) 2010, B. Scott Michel
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.scion.client;


import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;

/**
 * A project command group is a logical grouping of commands that excludes other commands from running
 * that operate on the same project or that project's resources.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public abstract class ProjectCommandGroup extends Job implements ISchedulingRule {
  /** The project against which this command group holds exclusive access */
  IProject theProject;
  
  public ProjectCommandGroup(String jobName, IProject theProject) {
    super(jobName);
    this.theProject = theProject;
    setRule(this);
  }
  
  /* (non-Javadoc)
   * @see org.eclipse.core.runtime.jobs.ISchedulingRule#contains(org.eclipse.core.runtime.jobs.ISchedulingRule)
   */
  public boolean contains(ISchedulingRule rule) {
    return (rule == this);
  }

  /**
   * Prevent two project command groups from running if they belong to the same
   * project. This predicate also knows about {@link FileCommandGroup) objects
   * and will prevent those from running as well.
   * 
   * @param rule The scheduling rule against which this rule is being compared.
   */
  public boolean isConflicting(ISchedulingRule rule) {
    boolean retval = false;
    
    if (rule instanceof ProjectCommandGroup) {
      ProjectCommandGroup projRule = (ProjectCommandGroup) rule;
      retval = (projRule == this || projRule.theProject == theProject);
    } else if (rule instanceof FileCommandGroup) {
      FileCommandGroup fileRule = (FileCommandGroup) rule;
      retval = ( fileRule.getProject() == theProject );
    }
    
    return retval;
  }
}
