package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import java.util.List;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;

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

    String oldModule = Util.getModuleName( file );
    String newName = getArguments().getNewName();
    IPath newPath = file.getProjectRelativePath().removeLastSegments( 1 )
        .append( newName );
    String newModule = Util.getModuleName( file.getProject(), newPath );

    // Create changes in module
    TextFileChange change = new TextFileChange( UITexts.renameParticipant_title, file );
    int moduleNamePos = Util.getModuleNameOffset( file );
    if (moduleNamePos != -1) {
      change.setEdit( new ReplaceEdit( moduleNamePos, oldModule.length(), newModule ) );
    }

    if( getArguments().getUpdateReferences() ) {

      // Update references
      boolean someChange = false;
      CompositeChange referencesChange = new CompositeChange( UITexts.updateReferences );
      for (IResource resource : Util.getHaskellFiles( file.getProject(), file )) {
        List<Integer> offsets = Util.getImportModuleOffsets( resource, oldModule );
        if (offsets.size() > 0) {
          TextFileChange importChanges = new TextFileChange( UITexts.updateReferences, (IFile)resource);
          MultiTextEdit multiEdit = new MultiTextEdit();
          for (int offset : offsets) {
            multiEdit.addChild( new ReplaceEdit( offset, oldModule.length(), newModule ) );
          }
          importChanges.setEdit( multiEdit );
          referencesChange.add( importChanges );
          someChange = true;
        }
      }

      // Cabal reference
      String newCabalFile = Util.newCabalFile( file.getProject(), file, newModule );
      IFile cabalF = ScionInstance.getCabalFile( file.getProject() );
      TextFileDocumentProvider provider = new TextFileDocumentProvider();
      TextFileChange cabalChanges = null;
      try {
        provider.connect( cabalF );
        IDocument doc = provider.getDocument( cabalF );
        int length = doc.getLength();
        if (!newCabalFile.equals( doc.get() )) {
          cabalChanges = new TextFileChange( UITexts.updateCabalFile, cabalF);
          cabalChanges.setEdit( new ReplaceEdit( 0, length, newCabalFile ) );
          someChange = true;
        }
        provider.disconnect( cabalF );
      } catch (Exception e) {
        // Do nothing
      }

      if (someChange) {
        return new CompositeChange( UITexts.renameParticipant_title, new Change[] { change, referencesChange, cabalChanges } );
      } else {
        return change;
      }
    } else {
      return change;
    }
  }

  @Override
  public Change createChange( final IProgressMonitor pm ) throws OperationCanceledException {
    return null;
  }
}
