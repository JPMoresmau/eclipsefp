package net.sf.eclipsefp.haskell.core.hlint;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.hlint.HLintRunner;
import net.sf.eclipsefp.haskell.hlint.Suggestion;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;


public class DeltaVisitor implements IResourceDeltaVisitor {

  @Override
  public boolean visit( final IResourceDelta delta ) throws CoreException {
    IResource resource = delta.getResource();
    if( HLintBuilder.mustBeVisited( resource ) ) {
      // We have to clean the previous markers
      resource.deleteMarkers( HaskellCorePlugin.ID_HLINT_MARKER, true,
          IResource.DEPTH_ZERO );
      // And add the new ones
      if( delta.getKind() == IResourceDelta.ADDED
          || delta.getKind() == IResourceDelta.CHANGED ) {
        for( Suggestion s: HLintRunner.runHLintOn( resource.getLocation() ) ) {
          HLintBuilder.createMarker( resource, s );
        }
      }
    }
    return true;
  }

}
