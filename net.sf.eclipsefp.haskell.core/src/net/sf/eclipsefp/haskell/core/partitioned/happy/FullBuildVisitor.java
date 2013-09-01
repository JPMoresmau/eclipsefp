package net.sf.eclipsefp.haskell.core.partitioned.happy;

import net.sf.eclipsefp.haskell.core.partitioned.runner.HappyRunner;
import net.sf.eclipsefp.haskell.core.partitioned.runner.ProcessorError;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
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
    if( HappyBuilder.mustBeVisited( resource ) ) {
      HappyRunner runner = new HappyRunner();
      for( ProcessorError s: runner.run( resource.getLocation() ) ) {
        HappyBuilder.createMarker( resource, s );
      }
      // Set derived file as derived
      resource.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
      IPath derivedPath = resource.getProjectRelativePath().removeFileExtension().addFileExtension( FileUtil.EXTENSION_HS );
      IFile f=resource.getProject().getFile( derivedPath );
      if (f.isAccessible()){
        f.setDerived( true,new NullProgressMonitor() );
      }
    }
    return true;
  }

}
