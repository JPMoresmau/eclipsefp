/**
 * 
 */
package net.sf.eclipsefp.haskell.scion.client;


import org.eclipse.core.resources.IFile;

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
    
    HaskellFileSchedulingRule rule = new HaskellFileSchedulingRule(theFile);
    
    setRule(rule);
    setName( adornedName(jobName, rule) );
    setPriority(priority);
  }
  
  /**
   * Get the command's file
   */
  public IFile getFile() {
    return theFile;
  }
}
