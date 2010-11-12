package net.sf.eclipsefp.haskell.scion.client;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

public abstract class WithLoadedFileCommandGroup extends FileCommandGroup {
  /**
   * Construct a command group that operates on a file loaded in the scion-server. If the file hasn't been loaded,
   * then arrange for it to be loaded before subsequent commands execute.
   * 
   * @param jobName
   *          The name of the job
   * @param theFile
   *          The file for which this command group is being executed.
   * @param priority 
   *          The Job's priority (should usually be Job.SHORT)
   */
  public WithLoadedFileCommandGroup(String jobName, IFile theFile, int priority) {
    super(jobName, theFile, priority);
    setPriority(priority);
  }

  /**
   * The run() method inherited from Job.
   */
  @Override
  protected IStatus run(IProgressMonitor monitor) {
    actions();
    return Status.OK_STATUS;
  }
  
  /**
   * The actions() method.
   */
  public abstract void actions();
}
