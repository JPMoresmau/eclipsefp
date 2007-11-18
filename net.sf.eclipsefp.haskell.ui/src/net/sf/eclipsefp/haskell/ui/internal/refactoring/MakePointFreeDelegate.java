// Copyright (c) 2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.refactoring;

import net.sf.eclipsefp.haskell.core.internal.refactoring.functions.IMakePointFree;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.IConditionChecker;
import org.eclipse.ltk.core.refactoring.participants.ValidateEditChecker;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;

import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

/** <p>delegate object that contains the logic used by the processor.</p>
  *
  * @author Leif Frenzel
  */
class MakePointFreeDelegate {
  
  private final MakePointFreeInfo info;
  private Change change;

  MakePointFreeDelegate( final MakePointFreeInfo info ) {
    this.info = info;
  }
  
  RefactoringStatus checkInitialConditions() {
    RefactoringStatus result = new RefactoringStatus();
    IFile sourceFile = info.getSourceFile();
    if( sourceFile == null || !sourceFile.exists() ) {
      result.addFatalError( UITexts.mkPointFreeDelegate_noSourceFile );
    } else if( info.getSourceFile().isReadOnly() ) {
      result.addFatalError( UITexts.mkPointFreeDelegate_roFile );
    } else if( isEmpty( info.getText() ) ) {
      result.addFatalError( UITexts.mkPointFreeDelegate_noSelection );
    }
    return result;
  }
  
  RefactoringStatus checkFinalConditions( final IProgressMonitor pm, 
                                          final CheckConditionsContext ctxt ) {
    RefactoringStatus result = new RefactoringStatus();
    try {
      pm.beginTask( UITexts.mkPointFreeDelegate_checking, 100 );
      if( ctxt != null ) {
        IConditionChecker checker = ctxt.getChecker( ValidateEditChecker.class );
        ValidateEditChecker editChecker = ( ValidateEditChecker )checker;
        editChecker.addFile( info.getSourceFile() );
      }
      change = createRenameChange();
      if( change == null ) {
        result.addFatalError( UITexts.mkPointFreeDelegate_notApplicable );
      }
    } finally {
      pm.done();
    }
    return result;
  }
  
  void createChange( final IProgressMonitor pm, 
                     final CompositeChange rootChange ) {
    try {
      pm.beginTask( UITexts.mkPointFreeDelegate_collectingChanges, 100 );
      if( change == null ) {
        throw new IllegalStateException();
      } 
      rootChange.add( change );
    } finally {
      pm.done();
    }
  }
  
  
  // helping methods
  //////////////////

  private Change createRenameChange() {
    TextFileChange result = null;
    CohatoeServer server = CohatoeServer.getInstance();
    Object fun = server.createFunction( IMakePointFree.class );
    String replacement = null;
    if( fun instanceof IMakePointFree ) {
      IMakePointFree primeFun = ( IMakePointFree )fun;
      replacement = primeFun.makePointFree( info.getText() );
    }
    if(    replacement != null 
        && !replacement.trim().equals( info.getText().trim() ) ) {
      IFile file = info.getSourceFile();
      result = new TextFileChange( file.getName(), file );
      // a file change contains a tree of edits, first add the root of them
      MultiTextEdit fileChangeRootEdit = new MultiTextEdit();
      result.setEdit( fileChangeRootEdit );    
      // edit object for the text replacement in the file, 
      // this is the only child
      ReplaceEdit edit = new ReplaceEdit( info.getOffset(), 
                                          info.getText().length(), 
                                          replacement );
      fileChangeRootEdit.addChild( edit );
    }
    return result;
  }
  
  private boolean isEmpty( final String candidate ) {
    return candidate == null || candidate.trim().length() == 0;
  }
}
