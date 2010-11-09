/**
 * 
 */
package net.sf.eclipsefp.haskell.scion.client;


import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

/**
 * A file command group is a Job that prevents other commands from interacting
 * with the scion-server that might operate on the same file resource.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public abstract class FileCommandGroup extends ProjectCommandGroup {
  /** The file for which this command group is being executed. */
  private IFile theFile;
  
  /**
   * Construct a file command group.
   * 
   * @param jobName
   *          The name of the job
   * @param theFile
   *          The file for which this command group is being executed.
   * @param priority TODO
   */
  public FileCommandGroup(String jobName, IFile theFile, int priority) {
    super(jobName, theFile.getProject(), false);
    this.theFile = theFile;
    setRule(this);
    setPriority(priority);
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
    
    if (rule instanceof FileCommandGroup) {
      FileCommandGroup fileGroup = (FileCommandGroup) rule;
      retval = retval || ( theFile.equals(fileGroup.theFile) && orderBefore(fileGroup));
    } else if (rule instanceof IResource) {
      IResource res = (IResource) rule;
      retval = retval || ( theFile.getFullPath().equals(res.getFullPath()) );
    }
    
    return retval;
  }
}
