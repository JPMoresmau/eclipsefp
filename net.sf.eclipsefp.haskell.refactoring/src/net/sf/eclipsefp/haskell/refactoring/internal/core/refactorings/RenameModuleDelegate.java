// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings;

import java.util.*;

import net.sf.eclipsefp.haskell.refactoring.internal.core.changes.RenameResourceChange;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.*;
import org.eclipse.text.edits.*;

import net.sf.eclipsefp.haskell.core.halamo.*;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;

/** <p>a delegate helper that creates changes for module renaming.</p>
  *
  * @author Leif Frenzel
  */
class RenameModuleDelegate {

  private final IRenameModuleInfo info;

  RenameModuleDelegate( final IRenameModuleInfo info ) {
    this.info = info;
  }
  
  void createChange( final CompositeChange change, final IProgressMonitor mon ) 
                                             throws OperationCanceledException {
    try {
      mon.beginTask( "Creating changes", 100 );
      IProgressMonitor subMon = new SubProgressMonitor( mon, 99 );
      createReplaceChanges( change, subMon );
      createRenameCUChange( change );
      mon.worked( 1 );
    } finally {
      mon.done();
    }
  }

  
  // helping methods
  //////////////////
  
  private void createReplaceChanges( final CompositeChange result,
                                     final IProgressMonitor mon ) {
    // files ==> TextFileChanges
    Map fileChanges = new HashMap();
    ICompilationUnit[] cus = getAllDependentCUs();

    mon.beginTask( "", cus.length + 1 );
    createRenameModuleChange( fileChanges );
    mon.worked( 1 );
    if( info.isUpdateReferences() ) {
      for( int i = 0; i < cus.length; i++ ) {
        IFile file = cus[ i ].getUnderlyingResource();
        IModule[] modules = cus[ i ].getModules();
        for( int j = 0; j < modules.length; j++ ) {
          createImportChange( file, modules[ j ], fileChanges );
        }
        mon.worked( 1 );
      }
    } else {
      mon.worked( cus.length );
    }
    Iterator iter = fileChanges.values().iterator();
    while( iter.hasNext() ) {
      result.add( ( Change )iter.next() );
    }
  }

  private void createImportChange( final IFile file, 
                                   final IModule module, 
                                   final Map fileChanges ) {
    IImport[] imports = module.getImports();
    for( int i = 0; i < imports.length; i++ ) {
      String name = imports[ i ].getName();
      if( name.equals( info.getOldName() ) ) {
        TextFileChange fileChange = getTextFileChange( fileChanges, file );
        try {
          int offset = computeOffset( imports[ i ], fileChange );
          ReplaceEdit replaceEdit = createReplaceEdit( offset );
          MultiTextEdit fileEdit = ( MultiTextEdit )fileChange.getEdit();
          fileEdit.addChild( replaceEdit );
          String importLabel = "Update import declaration";
          TextEditGroup textEditGroup = new TextEditGroup( importLabel,
                                                           replaceEdit );
          fileChange.addTextEditGroup( textEditGroup );
        } catch( final Exception ex ) {
          // ignore
        }
      }
    }
  }

  
  private int computeOffset( final IImport imp, final TextFileChange change )
                                                              throws Exception {
    int line = imp.getSourceLocation().getLine() - 1; // docs are zero-based
    int col = imp.getSourceLocation().getColumn() - 1; // docs are zero-based
    IDocument doc = change.getCurrentDocument( new NullProgressMonitor() );

    int lineOffset = doc.getLineOffset( line );
    int lineLength = doc.getLineLength( line );
    String candidate = doc.get( lineOffset, lineLength );
    return lineOffset + candidate.indexOf( info.getOldName(), col );
  }

  private ICompilationUnit[] getAllDependentCUs() {
    HaskellModelManager halamo = HaskellModelManager.getInstance();
    ICompilationUnit compilationUnit = halamo.getCompilationUnit( getFile() );
    return halamo.getDependentCUs( compilationUnit );
  }

  private ReplaceEdit createReplaceEdit( final int offset ) {
    int len = info.getOldName().length();
    String replacement = info.getNewName();
    return new ReplaceEdit( offset, len, replacement );
  }

  private void createRenameModuleChange( final Map fileChanges ) {
    TextFileChange fileChange = getTextFileChange( fileChanges, getFile() );

    ReplaceEdit replaceEdit = createReplaceEdit( info.getOffset() );
    MultiTextEdit fileEdit = ( MultiTextEdit )fileChange.getEdit();
    fileEdit.addChild( replaceEdit );
    String label = "Rename module in module declaration";
    fileChange.addTextEditGroup( new TextEditGroup( label, replaceEdit ) );
  }

  private void createRenameCUChange( final CompositeChange compChange ) {
    if( ResourceUtil.hasHaskellExtension( getFile() ) ) {
      String newName = info.getNewName() + determineExtension();
      Change renameChange = new RenameResourceChange( getFile(), newName );
      compChange.add( renameChange );
    }
  }

  private String determineExtension() {
    String result = ".";
    if( ResourceUtil.hasLiterateExtension( getFile() ) ) {
      result += ResourceUtil.EXTENSION_LHS;
    } else {
      result += ResourceUtil.EXTENSION_HS;
    }
    return result;
  }

  private TextFileChange getTextFileChange( final Map fileChanges,
                                            final IFile file ) {
    TextFileChange result;
    if( fileChanges.containsKey( file ) ) {
      result = ( TextFileChange )fileChanges.get( file );
    } else {
      result = new TextFileChange( file.getName(), file );
      MultiTextEdit fileEdit = new MultiTextEdit();
      result.setEdit( fileEdit );
      fileChanges.put( file, result );
    }
    return result;
  }

  private IFile getFile() {
    return info.getModule().getCompilationUnit().getUnderlyingResource();
  }
}
