package net.sf.eclipsefp.haskell.scion.client;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

public class HaskellProjectSchedulingRule extends CommandGroupSchedulingRule {
  private IProject theProject;
  
  public HaskellProjectSchedulingRule(final IProject theProject) {
    super();
    this.theProject = theProject;
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
