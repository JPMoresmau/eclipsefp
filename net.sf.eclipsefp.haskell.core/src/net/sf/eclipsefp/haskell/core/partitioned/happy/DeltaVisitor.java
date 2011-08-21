package net.sf.eclipsefp.haskell.core.partitioned.happy;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.partitioned.runner.HappyRunner;
import net.sf.eclipsefp.haskell.core.partitioned.runner.ProcessorError;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;


public class DeltaVisitor implements IResourceDeltaVisitor {

  public boolean visit( final IResourceDelta delta ) throws CoreException {
    IResource resource = delta.getResource();
    if( HappyBuilder.mustBeVisited( resource ) ) {
      // We have to clean the previous markers
      resource.deleteMarkers( HaskellCorePlugin.ID_HAPPY_MARKER, true,
          IResource.DEPTH_ZERO );
      // And add the new ones
      if( delta.getKind() == IResourceDelta.ADDED
          || delta.getKind() == IResourceDelta.CHANGED ) {
        HappyRunner runner = new HappyRunner();
        for( ProcessorError s: runner.run( resource.getLocation() ) ) {
          HappyBuilder.createMarker( resource, s );
        }
        // Set derived file as derived
        resource.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
        IPath derivedPath = resource.getProjectRelativePath().removeFileExtension().addFileExtension( FileUtil.EXTENSION_HS );
        resource.getProject().getFile( derivedPath ).setDerived( true, null );
      }
    }
    return true;
  }

}
