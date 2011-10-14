/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;

/**
 * Manages the copy of a Haskell source file.
 * @author Alejandro Serrano
 *
 */
public class CopyParticipant extends
    org.eclipse.ltk.core.refactoring.participants.CopyParticipant {

  IFile oldFile;

  public CopyParticipant() {
    // Do nothing
  }

  @Override
  protected boolean initialize( final Object element ) {
    if( element instanceof IFile ) {
      this.oldFile = ( IFile )element;
      return FileUtil.hasHaskellExtension( oldFile );
    }
    return false;
  }

  @Override
  public String getName() {
    return UITexts.copyParticipant_title;
  }

  @Override
  public RefactoringStatus checkConditions( final IProgressMonitor pm,
      final CheckConditionsContext context ) throws OperationCanceledException {
    return RefactoringStatus.create( Status.OK_STATUS );
  }

  @Override
  public Change createChange( final IProgressMonitor pm ) throws OperationCanceledException {
    // Get arguments
    Object dest=getArguments().getDestination();
    if (dest instanceof IFile){
      IFile newFile = (IFile)dest;
      return ChangeCreator.createCopyChange( oldFile, newFile );
    } else if (dest instanceof IFolder){
      IFile newFile = ((IFolder)dest).getFile( oldFile.getName() );
      return ChangeCreator.createCopyChange( oldFile, newFile );
    } else if (dest instanceof IProject){
      IFile newFile = ((IProject)dest).getFile( oldFile.getName() );
      return ChangeCreator.createCopyChange( oldFile, newFile );
    }
    return null;
  }

}
