package net.sf.eclipsefp.haskell.scion.internal.commands;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 * NOP (No operation, Z-80 assembly language) continuation. This is basically the null Job.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class NOPContinuation extends Job {
  public NOPContinuation() {
    super("NOP continuation");
  }
  
  @Override
  protected IStatus run(IProgressMonitor monitor) {
    return Status.OK_STATUS;
  }
}
