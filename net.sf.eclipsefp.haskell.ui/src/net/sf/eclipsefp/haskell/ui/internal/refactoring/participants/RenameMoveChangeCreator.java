package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import java.util.List;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;


public class RenameMoveChangeCreator {

  public static Change createChange(final IFile file, final IPath newPath, final boolean updateReferences, final String title) {
    String oldModule = Util.getModuleName( file );
    String newModule = Util.getModuleName( file.getProject(), newPath );

    // Create changes in module
    TextFileChange change = new TextFileChange( title, file );
    int moduleNamePos = Util.getModuleNameOffset( file );
    if (moduleNamePos != -1) {
      change.setEdit( new ReplaceEdit( moduleNamePos, oldModule.length(), newModule ) );
    }

    if( updateReferences ) {

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
        return new CompositeChange( title, new Change[] { change, referencesChange, cabalChanges } );
      } else {
        return change;
      }
    } else {
      return change;
    }
  }
}
