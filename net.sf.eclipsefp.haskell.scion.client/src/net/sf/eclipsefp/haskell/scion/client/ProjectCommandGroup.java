/**
 * (c) 2010, B. Scott Michel
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.scion.client;



import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

/**
 * A project command group is a logical grouping of commands that excludes other commands from running
 * that operate on the same project or that project's resources.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public abstract class ProjectCommandGroup extends CommandGroup {
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
   *          If true, installs the project-based command group scheduling rule.
   */
  protected ProjectCommandGroup(String jobName, IProject theProject, boolean installRule) {
    super(jobName);
    this.theProject = theProject;
    if (installRule) {
      setRule(this);
    }
  }

  /**
   * ISchedulingRule contains() method: This scheduling rule contains another
   * scheduling rule if the other is part of the same project.
   * 
   * @param rule
   *        The other scheduling rule
   * @return True if this scheduling rule and the other scheduling rule are part
   *         of the same project
   * 
   * @note Need to include this for
   *       {@link ScionInstance#buildProject(boolean, boolean)}, which has
   *       subsidiary Jobs that are contained within the same project.
   */
  @Override
  public boolean contains(ISchedulingRule rule) {
    boolean retval = super.contains(rule);
    
    if (rule instanceof ProjectCommandGroup) {
      retval = retval || ( theProject.equals( (ProjectCommandGroup) rule) );
    } else if (rule instanceof IProject) {
      retval = retval || theProject.equals( (IProject) rule );
    } else if (rule instanceof IResource ) {
      retval = retval || ( theProject.equals( ((IResource) rule).getProject() ) );
    }
    
    // This rule contains anotherProject if anotherProject is theProject or the rule
    // is this object. This ensures nesting of tasks within the same project.
    return retval;
  }

/**
   * Prevent two project command groups from running if they belong to the same
   * project. This predicate also knows about {@link FileCommandGroup) objects
   * and will prevent those from running as well.
   * 
   * @param rule The scheduling rule against which this rule is being compared.
   * @return True if the two rule conflict, i.e., belong to the same project.
   */
  @Override
  public boolean isConflicting(ISchedulingRule rule) {
    boolean retval = super.isConflicting(rule);
    
    if (rule instanceof ProjectCommandGroup) {
      ProjectCommandGroup projRule = (ProjectCommandGroup) rule;
      retval = retval || ( theProject.equals(projRule.theProject) );
    }
    
    return retval;
  }
}
