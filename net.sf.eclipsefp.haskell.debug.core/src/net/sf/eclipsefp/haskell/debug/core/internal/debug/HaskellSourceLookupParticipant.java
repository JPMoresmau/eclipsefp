package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant;

/**
 * Source lookup participant
 * @author JP Moresmau
 *
 */
public class HaskellSourceLookupParticipant extends
    AbstractSourceLookupParticipant {

  public String getSourceName( final Object object ) throws CoreException {
   if (object instanceof HaskellStrackFrame){
     return ((HaskellStrackFrame)object).getFileName();
   }
    return null;
  }

}
