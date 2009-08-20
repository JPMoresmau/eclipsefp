package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.core.project.IBuildTarget;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import org.eclipse.core.commands.operations.AbstractOperation;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;


public class AddBuildTargetOperation extends AbstractOperation {

  IHaskellProject project;
  IBuildTarget buildTarget;

  public AddBuildTargetOperation( final IHaskellProject project, final IBuildTarget buildTarget ) {
    super( "Creating build target" );
    this.project = project;
    this.buildTarget = buildTarget;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor, final IAdaptable info ) {
    project.addTarget( buildTarget );
    return Status.OK_STATUS;
  }

  @Override
  public IStatus redo( final IProgressMonitor monitor, final IAdaptable info ) {
    return execute(monitor, info);
  }

  @Override
  public IStatus undo( final IProgressMonitor monitor, final IAdaptable info ) {
    project.removeTarget( buildTarget );
    return Status.OK_STATUS;
  }

}
