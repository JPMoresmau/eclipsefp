package net.sf.eclipsefp.haskell.core.partitioned.uuagc;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;

/**
 * Performs an incremental build of the resources.
 *
 * @author Alejandro Serrano
 */
public class DeltaVisitor implements IResourceDeltaVisitor {

  @Override
  public boolean visit( final IResourceDelta delta ) throws CoreException {
    IResource resource = delta.getResource();
    if( UuagcBuilder.mustBeVisited( resource ) ) {
      // We have to clean the previous markers
      resource.deleteMarkers( HaskellCorePlugin.ID_UUAGC_MARKER, true,
          IResource.DEPTH_ZERO );
      // And add the new ones
      if( delta.getKind() == IResourceDelta.ADDED
          || delta.getKind() == IResourceDelta.CHANGED ) {
        UuagcBuilder.build( resource );
      }
    }
    return true;
  }

}
