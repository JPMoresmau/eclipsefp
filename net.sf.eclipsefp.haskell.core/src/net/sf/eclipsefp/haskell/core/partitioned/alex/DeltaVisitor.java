package net.sf.eclipsefp.haskell.core.partitioned.alex;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.partitioned.runner.AlexRunner;
import net.sf.eclipsefp.haskell.core.partitioned.runner.ProcessorError;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

/**
 * Performs an incremental build of the resources.
 *
 * @author Alejandro Serrano
 */
public class DeltaVisitor implements IResourceDeltaVisitor {

  public boolean visit( final IResourceDelta delta ) throws CoreException {
    IResource resource = delta.getResource();
    if( AlexBuilder.mustBeVisited( resource ) ) {
      // We have to clean the previous markers
      resource.deleteMarkers( HaskellCorePlugin.ID_ALEX_MARKER, true,
          IResource.DEPTH_ZERO );
      // And add the new ones
      if( delta.getKind() == IResourceDelta.ADDED
          || delta.getKind() == IResourceDelta.CHANGED ) {
        AlexRunner runner = new AlexRunner();
        for( ProcessorError s: runner.run( resource.getLocation() ) ) {
          AlexBuilder.createMarker( resource, s );
        }
        // Set derived file as derived
        resource.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
        IPath derivedPath = resource.getProjectRelativePath().removeFileExtension().addFileExtension( FileUtil.EXTENSION_HS );
        resource.getProject().getFile( derivedPath ).setDerived( true );
      }
    }
    return true;
  }

}
