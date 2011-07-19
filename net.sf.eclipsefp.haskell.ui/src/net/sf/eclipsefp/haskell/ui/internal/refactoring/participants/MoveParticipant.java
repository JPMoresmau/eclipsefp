package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;


public class MoveParticipant extends
    org.eclipse.ltk.core.refactoring.participants.MoveParticipant {

  IFile file;

  public MoveParticipant() {
    // Do nothing
  }

  @Override
  protected boolean initialize( final Object element ) {
    if( element instanceof IFile ) {
      this.file = ( IFile )element;
      return FileUtil.hasHaskellExtension( file );
    }
    return false;
  }

  @Override
  public String getName() {
    return UITexts.moveParticipant_title;
  }

  @Override
  public RefactoringStatus checkConditions( final IProgressMonitor pm,
      final CheckConditionsContext context ) throws OperationCanceledException {
    return RefactoringStatus.create( Status.OK_STATUS );
  }

  @Override
  public Change createPreChange( final IProgressMonitor pm ) throws OperationCanceledException {
    // Get arguments
    IPath newPath;
    Object destination = getArguments().getDestination();
    if (destination instanceof IProject) {
      IProject p = (IProject)destination;
      newPath = p.getProjectRelativePath().append( file.getProjectRelativePath().lastSegment() );
    } else {
      IFolder folder = (IFolder)getArguments().getDestination();
      newPath = folder.getProjectRelativePath().append( file.getProjectRelativePath().lastSegment() );
    }
    // Create change
    return ChangeCreator.createRenameMoveChange( file, newPath, getArguments().getUpdateReferences(), UITexts.moveParticipant_title );
  }

  @Override
  public Change createChange( final IProgressMonitor pm ) throws OperationCanceledException {
    return null;
  }

}
