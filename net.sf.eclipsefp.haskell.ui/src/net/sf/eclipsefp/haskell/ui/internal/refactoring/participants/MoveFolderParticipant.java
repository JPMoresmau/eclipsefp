/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import java.util.ArrayList;
import java.util.Set;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.MoveParticipant;

/**
 * Manages the move of a Haskell source folder.
 * @author Alejandro Serrano
 *
 */
public class MoveFolderParticipant extends MoveParticipant {

  IFolder folder;
  ArrayList<IFile> haskellFiles;

  public MoveFolderParticipant() {
    // Do nothing
  }

  @Override
  protected boolean initialize( final Object element ) {
    if( element instanceof IFolder ) {
      this.folder = (IFolder)element;
      // Precompute files to be changed
      this.haskellFiles = new ArrayList<>();
      try {
        folder.accept( new IResourceVisitor() {
          @Override
          public boolean visit( final IResource resource ) {
            if (resource instanceof IFile && FileUtil.hasHaskellExtension( resource )) {
              haskellFiles.add( (IFile )resource ); // Found a Haskell file
            }
            return true;
          }
        } );
      } catch (Exception e) {
        this.haskellFiles.clear();
      }
      return this.haskellFiles.size() > 0;
    }
    return false;
  }

  @Override
  public String getName() {
    return UITexts.moveFolderParticipant_title;
  }

  @Override
  public RefactoringStatus checkConditions( final IProgressMonitor pm,
      final CheckConditionsContext context ) throws OperationCanceledException {
    return RefactoringStatus.create( Status.OK_STATUS );
  }

  @Override
  public Change createPreChange( final IProgressMonitor pm ) throws OperationCanceledException {
    // Get arguments
    IPath oldPath = folder.getProjectRelativePath();
    IPath newPath;
    Object destination = getArguments().getDestination();
    if (destination instanceof IProject) {
      IProject p = (IProject)destination;
      newPath = p.getProjectRelativePath().append( oldPath.lastSegment() );
    } else {
      IFolder folder = (IFolder)getArguments().getDestination();
      newPath = folder.getProjectRelativePath().append( oldPath.lastSegment() );
    }

    // Check if it is one of the source folders
    Set<IPath> sourcePaths = Util.getPaths( folder.getProject() );
    if (sourcePaths.contains( oldPath )) {
      return ChangeCreator.createRenameMoveFolderCabalChange( folder.getProject(), oldPath, newPath );
    } else {
      // Create Haskell file change
      return ChangeCreator.createRenameMoveFolderChange( haskellFiles, folder.getProjectRelativePath(),
          newPath, getArguments().getUpdateReferences(), UITexts.renameFolderParticipant_title, UITexts.renameParticipant_title );
    }
  }

  @Override
  public Change createChange( final IProgressMonitor pm ) throws OperationCanceledException {
    return null;
  }

}
