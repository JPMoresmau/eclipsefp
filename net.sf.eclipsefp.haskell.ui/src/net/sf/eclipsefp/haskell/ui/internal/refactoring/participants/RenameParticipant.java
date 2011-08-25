/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;

/**
 * Manages the renaming of a Haskell source file.
 * @author Alejandro Serrano
 *
 */
public class RenameParticipant extends
    org.eclipse.ltk.core.refactoring.participants.RenameParticipant {

  IFile file;

  public RenameParticipant() {
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
    return UITexts.renameParticipant_title;
  }

  @Override
  public RefactoringStatus checkConditions( final IProgressMonitor pm,
      final CheckConditionsContext context ) throws OperationCanceledException {
    return RefactoringStatus.create( Status.OK_STATUS );
  }

  @Override
  public Change createPreChange( final IProgressMonitor pm ) throws OperationCanceledException {
    // Get arguments
    String newName = getArguments().getNewName();
    IPath newPath = file.getProjectRelativePath().removeLastSegments( 1 )
        .append( newName );
    // Create change
    return ChangeCreator.createRenameMoveChange( file, newPath, getArguments().getUpdateReferences(), UITexts.renameParticipant_title );
  }

  @Override
  public Change createChange( final IProgressMonitor pm ) throws OperationCanceledException {
    return null;
  }
}
