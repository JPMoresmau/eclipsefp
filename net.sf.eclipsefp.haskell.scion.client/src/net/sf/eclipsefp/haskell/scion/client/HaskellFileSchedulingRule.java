package net.sf.eclipsefp.haskell.scion.client;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

public class HaskellFileSchedulingRule extends HaskellProjectSchedulingRule {
  private IFile theFile;
  
  public HaskellFileSchedulingRule(final IFile theFile) {
    super(theFile.getProject());
    this.theFile = theFile;
  }
  /**
   * ISchedulingRule contains() method: This method ensures that this scheduling rule only
   * contains itself.
   * 
   * @param rule The other scheduling rule
   * @return True iff the other scheduling rule is the same as this scheduling rule.
   */
  public boolean contains(ISchedulingRule rule) {
    return (rule == this);
  }

  /**
   * Prevent another command group from running if it is associated with the same file or
   * within the same project (the superclass enforces this.)
   * 
   * @param rule The other scheduling rule.
   * @return True if the other scheduling rule conflicts.
   */
  @Override
  public boolean isConflicting(ISchedulingRule rule) {
    boolean retval = super.isConflicting(rule);
    
    if (rule instanceof HaskellFileSchedulingRule) {
      HaskellFileSchedulingRule fileGroup = (HaskellFileSchedulingRule) rule;
      retval = retval || ( theFile.equals(fileGroup.theFile) && orderBefore(fileGroup));
    } else if (rule instanceof IResource) {
      IResource res = (IResource) rule;
      retval = retval || ( theFile.getFullPath().equals(res.getFullPath()) );
    }
    
    return retval;
  }
}
