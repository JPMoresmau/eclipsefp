// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring;

import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.Occurrence;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPoint;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageQueryFlags;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.handlers.ReferencesWorkspaceHandler;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.participants.ChangeCreator;
import net.sf.eclipsefp.haskell.ui.internal.search.UsageQuery;
import net.sf.eclipsefp.haskell.ui.internal.search.UsageSearchResult;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.IConditionChecker;
import org.eclipse.ltk.core.refactoring.participants.ValidateEditChecker;

/** <p>delegate object that contains the logic used by the processor.</p>
  *
  * @author Leif Frenzel
  */
public class RenameDelegate extends RefDelegate {

  private CompositeChange change;

  private String newName;
  private IProject project;

  public RenameDelegate( final RefInfo info ) {
    super( info );
    info.setAllowEmptySelection( true );
    ThingAtPoint tap=getThingAtPoint();
    if (tap!=null){
      newName=tap.getName();
      project=info.getSourceFile().getProject();
    }
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
      change = createRenameChange(pm);
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
      // do not have the intermediate step
      for (Change c:change.getChildren()){
        change.remove( c );
        rootChange.add( c );
      }
      //rootChange.add( change );
    } finally {
      pm.done();
    }
  }


  // helping methods
  //////////////////

  private CompositeChange createRenameChange(final IProgressMonitor pm) {
    CompositeChange result = null;
    if (info.getTargetEditor() instanceof HaskellEditor){
      final HaskellEditor haskellEditor= (HaskellEditor)info.getTargetEditor();
      try {
        Location l=getLocation();
        if (l!=null){
          ThingAtPoint tap=getThingAtPoint(l);
          if (tap!=null){
              UsageQuery uq=ReferencesWorkspaceHandler.getUsageQuery( haskellEditor, getProject(), tap );
              uq.setScopeFlags( UsageQueryFlags.SCOPE_ALL );
              uq.run( pm );
              UsageSearchResult usr=(UsageSearchResult)uq.getSearchResult();
              if (usr.getResults().getSize()>0){
                CompositeChange cc=ChangeCreator.getReferencesChange( usr.getResults(), tap.getName(), getNewName() );
                return cc;
              } else {
                // nothing found: change locally
                List<Occurrence> occs=haskellEditor.getTokenScanner().getOccurrences( l.getStartOffset( haskellEditor.getDocument() ) );
                Location spanLocation=haskellEditor.getOutlineSpan( getLocation().getStartOffset( haskellEditor.getDocument() ) );
                CompositeChange cc=ChangeCreator.getLocalReferencesChange(haskellEditor.findFile(),spanLocation, occs, tap.getName(), getNewName() );
                return cc;
              }
          }
        }
      } catch (Exception e){
        HaskellUIPlugin.log( e );
      }

    }
    // TODO TtC replace by something not Cohatoe-based
    /*
    CohatoeServer server = CohatoeServer.getInstance();
    IRename fun = server.createFunction( IRename.class );
    if( fun != null ) {
      String newName = "I-AM-THE-NEW-NAME";
      int line = info.getLine();
      int column = info.getColumn();
      List<IReplaceEditDesc> descs
        = fun.performRename( info.getSourceFile(), line, column, newName );
      Map<IFile, List<IReplaceEditDesc>> map = mapByFile( descs );
      for( IFile file: map.keySet() ) {
        result = new TextFileChange( file.getName(), file );
        // a file change contains a tree of edits, first add the root of them
        MultiTextEdit fileChangeRootEdit = new MultiTextEdit();
        result.setEdit( fileChangeRootEdit );
        // edit object for the text replacement in the file,
        // this is the only child
        List<IReplaceEditDesc> editDescs = map.get( file );
        for( IReplaceEditDesc editDesc: editDescs ) {
          ReplaceEdit edit = new ReplaceEdit( editDesc.getOffset(),
                                              editDesc.getLength(),
                                              editDesc.getReplacement() );
          fileChangeRootEdit.addChild( edit );
        }
      }
    }
    */
    return result;
  }




  public String getNewName() {
    return newName;
  }




  public void setNewName( final String newName ) {
    this.newName = newName;
  }




  public IProject getProject() {
    return project;
  }




  public void setProject( final IProject project ) {
    this.project = project;
  }

  /*
  private Map<IFile, List<IReplaceEditDesc>> mapByFile(
      final List<IReplaceEditDesc> editDescs ) {
    Map<IFile, List<IReplaceEditDesc>> result
      = new HashMap<IFile, List<IReplaceEditDesc>>();
    for( IReplaceEditDesc editDesc: editDescs ) {
      List<IReplaceEditDesc> list = result.get( editDesc.getFile() );
      if( list == null ) {
        list = new ArrayList<IReplaceEditDesc>();
        result.put( editDesc.getFile(), list );
      }
      list.add( editDesc );
    }
    return result;
  }
  */
}
