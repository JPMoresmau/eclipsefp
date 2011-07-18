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
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.text.edits.ReplaceEdit;

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
    if( getArguments().getUpdateReferences() ) {
      // Check if things can be done
      return RefactoringStatus.create( Status.OK_STATUS );
    } else {
      // No changes
      return RefactoringStatus.create( Status.OK_STATUS );
    }
  }

  @Override
  public Change createPreChange( final IProgressMonitor pm ) throws OperationCanceledException {
    if( getArguments().getUpdateReferences() ) {
      String oldModule = Util.getModuleName( file );
      String newName = getArguments().getNewName();
      IPath newPath = file.getProjectRelativePath().removeLastSegments( 1 )
          .append( newName );
      String newModule = Util.getModuleName( file.getProject(), newPath );

      int moduleNamePos = Util.getModuleNameOffset( file );
      if (moduleNamePos != -1) {
        TextFileChange change = new TextFileChange( UITexts.renameParticipant_title, file );
        change.setEdit( new ReplaceEdit( moduleNamePos, oldModule.length(), newModule ) );
        return change;
      }
      // Create changes
      return null;
    } else {
      // No changes
      return null;
    }
  }

  @Override
  public Change createChange( final IProgressMonitor pm ) throws OperationCanceledException {
    return null;
  }
}
