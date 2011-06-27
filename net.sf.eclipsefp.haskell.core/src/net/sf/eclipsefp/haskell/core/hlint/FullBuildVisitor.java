package net.sf.eclipsefp.haskell.core.hlint;

import net.sf.eclipsefp.haskell.hlint.HLintRunner;
import net.sf.eclipsefp.haskell.hlint.Suggestion;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;


public class FullBuildVisitor implements IResourceVisitor {

  @Override
  public boolean visit( final IResource resource ) throws CoreException {
    if( HLintBuilder.mustBeVisited( resource ) ) {
      for( Suggestion s: HLintRunner.runHLintOn( resource.getLocation() ) ) {
        HLintBuilder.createMarker( resource, s );
      }
    }
    return true;
  }

}
