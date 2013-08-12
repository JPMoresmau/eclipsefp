package net.sf.eclipsefp.haskell.core.partitioned.uuagc;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

/**
 * Builds all resources in the project.
 *
 * @author Alejandro Serrano
 */
public class FullBuildVisitor implements IResourceVisitor {

  @Override
  public boolean visit( final IResource resource ) throws CoreException {
    if( UuagcBuilder.mustBeVisited( resource ) ) {
     UuagcBuilder.build( resource );
    }
    return true;
  }

}
