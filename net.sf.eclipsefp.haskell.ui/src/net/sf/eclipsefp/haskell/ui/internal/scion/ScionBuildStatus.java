package net.sf.eclipsefp.haskell.ui.internal.scion;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/** State container that keeps track of the built-in Scion server's build status
  *
  * @author B. Scott Michel (scooter.phd@gmail.com)
  */
public class ScionBuildStatus {
  private IStatus status;
  private IPath executableName;
  private String title;
  private String message;

  public ScionBuildStatus() {
    this.status = Status.OK_STATUS;
  }

  public void buildFailed(final String title, final String message) {
    status = Status.CANCEL_STATUS;
    this.title = title;
    this.message = message;
  }

  public final void setExecutable(final IPath executable) {
    executableName = executable;
  }

  public final IStatus getStatus() {
    return status;
  }

  public final IPath getExecutable() {
    return executableName;
  }

  public final String getTitle() {
    return title;
  }

  public final String getMessage() {
    return message;
  }

  public final boolean isOK() {
    return status.equals( Status.OK_STATUS );
  }
}
