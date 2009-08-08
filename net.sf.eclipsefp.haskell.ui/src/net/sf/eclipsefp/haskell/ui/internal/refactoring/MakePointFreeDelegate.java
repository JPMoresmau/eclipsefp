// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.IConditionChecker;
import org.eclipse.ltk.core.refactoring.participants.ValidateEditChecker;

/** <p>delegate object that contains the logic used by the processor.</p>
  *
  * @author Leif Frenzel
  */
public class MakePointFreeDelegate extends RefDelegate {

  private Change change;

  public MakePointFreeDelegate( final RefInfo info ) {
    super( info );
    info.setAllowEmptySelection( false );
  }

  @Override
  RefactoringStatus checkFinalConditions( final IProgressMonitor pm,
                                          final CheckConditionsContext ctxt ) {
    RefactoringStatus result = new RefactoringStatus();
    try {
      pm.beginTask( UITexts.refDelegate_checking, 100 );
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

  @Override
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
    // TODO TtC replace by something not Cohatoe-based
    /*
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
    */
    return result;
  }

}
