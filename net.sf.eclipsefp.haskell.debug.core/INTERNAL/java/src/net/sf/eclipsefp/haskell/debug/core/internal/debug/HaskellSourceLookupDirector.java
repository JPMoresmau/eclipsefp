package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;

/**
 * Source lookup director for Haskell
 * @author JP Moresmau
 *
 */
public class HaskellSourceLookupDirector extends AbstractSourceLookupDirector {


  public void initializeParticipants() {
   addParticipants( new ISourceLookupParticipant[]{new HaskellSourceLookupParticipant()} );
  }

}
