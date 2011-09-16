/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;

/**
 * Common methods for creating changes in participants, because
 * most of them are reused between file and folder operations.
 * @author Alejandro Serrano
 *
 */
public class ChangeCreator {

  public static TextFileChange createCopyChange(final IFile oldFile, final IFile newFile) {
    String oldModule = Util.getModuleName( oldFile );
    String newModule = Util.getModuleName( newFile );

    // Create changes in module
    TextFileChange change = new TextFileChange( UITexts.copyParticipant_title, newFile );
    int moduleNamePos = Util.getModuleNameOffset( newFile );
    if (moduleNamePos != -1) {
      change.setEdit( new ReplaceEdit( moduleNamePos, oldModule.length(), newModule ) );
    }

    return change;
  }

  public static CompositeChange createRenameMoveFolderChange(final List<IFile> files,
      final IPath oldPath, final IPath newPath, final boolean updateReferences,
      final String compositeTitle, final String elementTitle) {
    CompositeChange change = new CompositeChange( compositeTitle );
    for (IFile file : files) {
      IPath oldFilePath = file.getProjectRelativePath();
      IPath noPrefixPath = oldFilePath.removeFirstSegments( oldPath.segmentCount() );
      IPath newFilePath = newPath.append( noPrefixPath );
      Change fileChange = createRenameMoveChange( file, newFilePath, updateReferences, elementTitle );
      change.add( fileChange );
    }
    return change;
  }

  public static Change createRenameMoveFolderCabalChange(final IProject project, final IPath oldPath, final IPath newPath) {
    // Cabal reference
    String newCabalFile = Util.newSourceFolderCabalFile( project, oldPath, newPath );
    IFile cabalF = BuildWrapperPlugin.getCabalFile( project );
    TextFileDocumentProvider provider = new TextFileDocumentProvider();
    TextFileChange cabalChanges = null;
    try {
      provider.connect( cabalF );
      IDocument doc = provider.getDocument( cabalF );
      int length = doc.getLength();
      if (!newCabalFile.equals( doc.get() )) {
        cabalChanges = new TextFileChange( UITexts.updateCabalFile, cabalF);
        cabalChanges.setEdit( new ReplaceEdit( 0, length, newCabalFile ) );
      }
    } catch (Exception e) {
      cabalChanges = null;
    } finally {
      provider.disconnect( cabalF );
    }

    return cabalChanges;
  }

  public static Change createRenameMoveChange(final IFile file, final IPath newPath,
      final boolean updateReferences, final String title) {
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
      IFile cabalF = BuildWrapperPlugin.getCabalFile( file.getProject() );
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

  public static Change createRenameMoveInOtherProjectChange(final IFile file, final IPath newPath,
      final boolean updateReferences, final String title) {
    String oldModule = Util.getModuleName( file );
    String newModule = Util.getModuleName( file.getProject(), newPath );

    // Create changes in module
    TextFileChange change = new TextFileChange( title, file );
    int moduleNamePos = Util.getModuleNameOffset( file );
    if (moduleNamePos != -1) {
      change.setEdit( new ReplaceEdit( moduleNamePos, oldModule.length(), newModule ) );
    }

    if( updateReferences ) {

      boolean someChange = false;

      // Cabal reference
      String newCabalFile = Util.newRemoveModuleCabalFile( file.getProject(), file );
      IFile cabalF = BuildWrapperPlugin.getCabalFile( file.getProject() );
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
        return new CompositeChange( title, new Change[] { change, cabalChanges } );
      } else {
        return change;
      }
    } else {
      return change;
    }
  }
}
