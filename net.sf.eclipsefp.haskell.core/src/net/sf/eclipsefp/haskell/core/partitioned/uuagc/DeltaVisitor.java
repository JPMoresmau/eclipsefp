package net.sf.eclipsefp.haskell.core.partitioned.uuagc;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.partitioned.runner.ProcessorError;
import net.sf.eclipsefp.haskell.core.partitioned.runner.UuagcRunner;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;

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
        UuagcRunner runner = new UuagcRunner( resource.getProject() );
        for( ProcessorError s: runner.run( resource ) ) {
          UuagcBuilder.createMarker( resource, s );
        }
        // Set derived file as derived
        resource.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
        IPath derivedPath = resource.getProjectRelativePath()
            .removeFileExtension().addFileExtension( FileUtil.EXTENSION_HS );
        resource.getProject().getFile( derivedPath ).setDerived( true,new NullProgressMonitor() );
      }
    }
    return true;
  }

}
