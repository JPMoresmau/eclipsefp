// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPoint;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.Region;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;

/** <p>super class for refactoring delegates, encapsulates some common
  * functionality.</p>
  *
  * @author Leif Frenzel
  */
abstract class RefDelegate {

  protected final RefInfo info;
  private ThingAtPoint tap;

  RefDelegate( final RefInfo info ) {
    this.info = info;
  }

  RefactoringStatus checkInitialConditions() {
    RefactoringStatus result = new RefactoringStatus();
    IFile sourceFile = info.getSourceFile();
    if( sourceFile == null || !sourceFile.exists() ) {
      result.addFatalError( UITexts.refDelegate_noSourceFile );
    } else if( info.getSourceFile().isReadOnly() ) {
      result.addFatalError( UITexts.refDelegate_roFile );
    } else if( !info.isAllowEmptySelection() && isEmpty( info.getText() ) ) {
      result.addFatalError( UITexts.refDelegate_noSelection );
    }
    return result;
  }


  /**
   * @return the info
   */
  public RefInfo getInfo() {
    return info;
  }

  public ThingAtPoint getThingAtPoint(){
    if (tap==null){
      if (info.getTargetEditor() instanceof HaskellEditor){
        final HaskellEditor haskellEditor= (HaskellEditor)info.getTargetEditor();
        try {
          Location l = new Location( info.getSourceFile().getLocation().toOSString(),
              haskellEditor.getDocument(), new Region( info.getOffset(), 0 ) );
          BWFacade f=BuildWrapperPlugin.getFacade( info.getSourceFile().getProject() );
          tap=f.getThingAtPoint( info.getSourceFile(), l );
        } catch (Exception e){
          HaskellUIPlugin.log( e );
        }
      }
    }
    return tap;
  }

  abstract RefactoringStatus checkFinalConditions(
    final IProgressMonitor pm,
    final CheckConditionsContext ctxt );

  abstract void createChange( final IProgressMonitor pm, final CompositeChange rootChange );


  // helping methods
  //////////////////

  private boolean isEmpty( final String candidate ) {
    return candidate == null || candidate.trim().length() == 0;
  }
}
