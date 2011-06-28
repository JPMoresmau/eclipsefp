package net.sf.eclipsefp.haskell.core.hlint;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;


public class CleanVisitor implements IResourceVisitor {

  public boolean visit( final IResource resource ) throws CoreException {
    if( HLintBuilder.mustBeVisited( resource ) ) {
      // We have to clean the previous markers
      resource.deleteMarkers( HaskellCorePlugin.ID_HLINT_MARKER, true,
          IResource.DEPTH_ZERO );
    }
    return true;
  }

}
