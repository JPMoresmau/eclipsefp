// Copyright (c) 2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.refactoring;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.RefactoringParticipant;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.core.refactoring.participants.SharableParticipants;

/** <p>The processor is where the work is delegated to if participants are
  * involved. The processor loads the participants and manages the lifecycle
  * of the refactoring. In order to do that, the refactoring entry point
  * methods must be implemented.</p>
  *
  * @author Leif Frenzel
  */
public class MakePointFreeProcessor extends RefactoringProcessor {

  private final MakePointFreeDelegate delegate;

  public MakePointFreeProcessor( final MakePointFreeInfo info ) {
    delegate = new MakePointFreeDelegate( info );
  }


  // interface methods of RefactoringProcessor
  ////////////////////////////////////////////

  @Override
  public Object[] getElements() {
    return new Object[] { UITexts.mkPointFreeProcessor_elem };
  }

  @Override
  public String getIdentifier() {
    return getClass().getName();
  }

  @Override
  public String getProcessorName() {
    return UITexts.mkPointFreeProcessor_name;
  }

  @Override
  public boolean isApplicable() {
    return true;
  }

  @Override
  public RefactoringStatus checkInitialConditions( final IProgressMonitor pm ) {
    return delegate.checkInitialConditions();
  }

  @Override
  public RefactoringStatus checkFinalConditions(
            final IProgressMonitor pm,  final CheckConditionsContext context ) {
    return delegate.checkFinalConditions( pm, context );
  }

  @Override
  public Change createChange( final IProgressMonitor pm ) {
    CompositeChange result = new CompositeChange( getProcessorName() );
    delegate.createChange( pm, result );
    return result;
  }

  @Override
  public RefactoringParticipant[] loadParticipants(
                               final RefactoringStatus status,
                               final SharableParticipants sharedParticipants ) {
    // This would be the place to load the participants via the
    // ParticipantManager and decide which of them are allowed to participate.
    return new RefactoringParticipant[ 0 ];
  }
}
