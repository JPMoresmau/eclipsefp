package net.sf.eclipsefp.haskell.core.partitioned.uuagc;

import net.sf.eclipsefp.haskell.core.partitioned.runner.ProcessorError;
import net.sf.eclipsefp.haskell.core.partitioned.runner.UuagcRunner;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;

/**
 * Builds all resources in the project.
 *
 * @author Alejandro Serrano
 */
public class FullBuildVisitor implements IResourceVisitor {

  @Override
  public boolean visit( final IResource resource ) throws CoreException {
    if( UuagcBuilder.mustBeVisited( resource ) ) {
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
    return true;
  }

}
