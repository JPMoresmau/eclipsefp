// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings;

import org.eclipse.core.runtime.*;
import org.eclipse.ltk.core.refactoring.*;
import org.eclipse.ltk.core.refactoring.participants.*;

/** <p>The refactoring processor for renaming Haskell modules in the 
  * workspace. Renaming modules involves renaming source files, changing
  * the module declaration, and updating import statements in dependent 
  * compilation units.</p>
  *
  * @author Leif Frenzel
  */
public class RenameModuleProcessor extends RenameProcessor {

  private final IRenameModuleInfo info;

  public RenameModuleProcessor( final IRenameModuleInfo info ) {
    this.info = info;
  } 

  public IRenameModuleInfo getInfo() {
    return info;
  }

  
  // interface methods of RenameProcessor
  ///////////////////////////////////////
  
  public Object[] getElements() {
    return new Object[] { info.getModule() };
  }

  public String getIdentifier() {
    return getClass().getName();
  }

  public String getProcessorName() {
    return "Rename Haskell Module";
  }

  public boolean isApplicable() throws CoreException {
    // TODO maybe more fine-grained here? Whn is this called?
    return true;
  }

  public RefactoringStatus checkInitialConditions( final IProgressMonitor mon )
                              throws CoreException, OperationCanceledException {
System.err.println( "checkInitialConditions" );
// TODO
    return new RefactoringStatus();
  }

  public RefactoringStatus checkFinalConditions( 
                    final IProgressMonitor mon,
                    final CheckConditionsContext context ) 
                              throws CoreException, OperationCanceledException {
System.err.println( "checkFinalConditions" );
// TODO
    return new RefactoringStatus();
  }

  public Change createChange( final IProgressMonitor mon ) 
                              throws CoreException, OperationCanceledException {
    CompositeChange result = new CompositeChange( getProcessorName() );
    new RenameModuleDelegate( getInfo() ).createChange( result, mon );
    return result;
  }

  public RefactoringParticipant[] loadParticipants( 
          final RefactoringStatus status,
          final SharableParticipants sharedParticipants ) throws CoreException {
    // TODO
    return new RefactoringParticipant[ 0 ];
  }
}
