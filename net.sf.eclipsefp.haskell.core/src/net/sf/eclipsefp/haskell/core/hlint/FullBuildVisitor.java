package net.sf.eclipsefp.haskell.core.hlint;

import net.sf.eclipsefp.haskell.hlint.HLintRunner;
import net.sf.eclipsefp.haskell.hlint.Suggestion;
import org.eclipse.core.resources.IFile;
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
    IFile f=HLintBuilder.getFileToVisit( resource );
    if( f!=null) {
      for( Suggestion s: HLintRunner.runHLintOn( resource.getProject().getLocation(), f ) ) {
        HLintBuilder.createMarker( resource, s );
      }
    }
    return true;
  }

}
