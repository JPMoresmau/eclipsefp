/**
 * 
 */
package net.sf.eclipsefp.haskell.scion.client;


import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;

/**
 * A file command group prevents other commands from interacting with the scion-server that might
 * operate on the same file resource.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public abstract class FileCommandGroup extends Job implements ISchedulingRule {
  /** The file for which this command group is being executed. */
  private IFile theFile;
  
  /**
   * Construct a file command group.
   * 
   * @param jobName
   *          The name of the job
   * @param theFile
   *          The file for which this command group is being executed.
   */
  public FileCommandGroup(String jobName, IFile theFile) {
    super(jobName);
    this.theFile = theFile;
    setRule(this);
  }
  
  /**
   * Get the file's associated project, used by {@link ProjectCommandGroup} to prevent
   * this file command group from conflicting with project command groups.
   * 
   * @return The file's project.
   */
  public IProject getProject() {
    return theFile.getProject();
  }
  
  /* (non-Javadoc)
   * @see org.eclipse.core.runtime.jobs.ISchedulingRule#contains(org.eclipse.core.runtime.jobs.ISchedulingRule)
   */
  public boolean contains(ISchedulingRule rule) {
    return (rule == this);
  }

  /**
   * Prevent another file command group from running if it is associated with the same file.
   */
  public boolean isConflicting(ISchedulingRule rule) {
    boolean retval = false;
    
    if (rule instanceof FileCommandGroup) {
      FileCommandGroup fileGroup = (FileCommandGroup) rule;
      retval = (rule == this || fileGroup.theFile == theFile);
    }
    
    return retval;
  }
}
