/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.SearchResultLocation;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * Common methods for creating changes in participants, because
 * most of them are reused between file and folder operations.
 * @author Alejandro Serrano
 * @author JP Moresmau
 */
public class ChangeCreator {

  public static TextFileChange createCopyChange(final IFile oldFile, final IFile newFile) {
    String oldModule = ResourceUtil.getModuleName( oldFile );
    String newModule = ResourceUtil.getModuleName( newFile );

    // Create changes in module
    TextFileChange change = new TextFileChange( UITexts.copyParticipant_title, newFile );
    int moduleNamePos = Util.getModuleNameOffset( newFile,oldModule  );
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
    String oldModule = ResourceUtil.getModuleName( file );
    String newModule = ResourceUtil.getModuleName(file.getProject().getFile( newPath ));

    // Create changes in module
    TextFileChange change = new TextFileChange( title, file );
    int moduleNamePos = Util.getModuleNameOffset( file,oldModule );
    if (moduleNamePos != -1) {
      change.setEdit( new ReplaceEdit( moduleNamePos, oldModule.length(), newModule ) );
    }

    if( updateReferences ) {

      // Update references
      UsageResults ur=BuildWrapperPlugin.getDefault().getUsageAPI().getModuleReferences( null, oldModule, null, true );
      boolean someChange = ur.getSize()>0;
      CompositeChange referencesChange = new CompositeChange( UITexts.updateReferences );
      for (IProject refP:ur.listProjects()){
        Map<IFile,Map<String,Collection<SearchResultLocation>>> um =ur.getUsageInProject( refP );
        for (IFile f:um.keySet()){
          TextFileChange importChanges = new TextFileChange( UITexts.updateReferences,f);

          MultiTextEdit multiEdit = new MultiTextEdit();
          IDocumentProvider prov=new TextFileDocumentProvider();
          try {
            prov.connect( f );
            IDocument doc=prov.getDocument(  f );
            try {
              for (Collection<SearchResultLocation> csrl:um.get( f ).values()){
                for (SearchResultLocation srl:csrl){
                  int offset=srl.getStartOffset( doc );
                  multiEdit.addChild( new ReplaceEdit( offset, oldModule.length(), newModule ));
                }
              }
            } finally {
              prov.disconnect( f );
            }
          } catch (Exception ce){
            HaskellUIPlugin.log( ce );
          }
          importChanges.setEdit( multiEdit );
          referencesChange.add( importChanges );
        }
      }

      /*for (IResource resource : Util.getHaskellFiles( file.getProject(), file )) {
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
      }*/

      // Cabal reference
      String newCabalFile = Util.newCabalFile( file.getProject(), file, newModule );
      IFile cabalF = BuildWrapperPlugin.getCabalFile( file.getProject() );
      TextFileDocumentProvider provider = new TextFileDocumentProvider();
      TextFileChange cabalChanges = null;
      try {
        provider.connect( cabalF );
        try {
          IDocument doc = provider.getDocument( cabalF );
          int length = doc.getLength();
          if (!newCabalFile.equals( doc.get() )) {
            cabalChanges = new TextFileChange( UITexts.updateCabalFile, cabalF);
            cabalChanges.setEdit( new ReplaceEdit( 0, length, newCabalFile ) );
            someChange = true;
          }
        } finally {
          provider.disconnect( cabalF );
        }
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
    String oldModule = ResourceUtil.getModuleName( file );
    String newModule = ResourceUtil.getModuleName( file.getProject().getFile( newPath));

    // Create changes in module
    TextFileChange change = new TextFileChange( title, file );
    int moduleNamePos = Util.getModuleNameOffset( file,oldModule  );
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
