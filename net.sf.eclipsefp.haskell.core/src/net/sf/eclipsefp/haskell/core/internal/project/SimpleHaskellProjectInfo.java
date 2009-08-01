package net.sf.eclipsefp.haskell.core.internal.project;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.project.IBuildTarget;
import net.sf.eclipsefp.haskell.core.project.IHaskellProjectInfo;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * A {@link IHaskellProjectInfo} without a Cabal file.
 * Writes the information to the .hsproject file.
 *
 * @author Thomas ten Cate
 */
public class SimpleHaskellProjectInfo implements IHaskellProjectInfo {

  private final Set<IPath> sourcePaths = new HashSet<IPath>();
  private final Set<IBuildTarget> targets = new HashSet<IBuildTarget>();
  private IPath outputPath = new Path( "" ); //$NON-NLS-1$
  private IPath binPath = new Path( "" ); //$NON-NLS-1$

  public void addSourcePath( final IPath path ) {
    check( path );
    sourcePaths.add( path );
  }

  public void addTarget( final IBuildTarget target ) {
    check( target );
    targets.add( target );
  }

  public IPath getBinPath() {
    return binPath;
  }

  public IPath getOutputPath() {
    return outputPath;
  }

  public Set<IPath> getSourcePaths() {
    return Collections.unmodifiableSet( sourcePaths );
  }

  public Set<IBuildTarget> getTargets() {
    return Collections.unmodifiableSet( targets );
  }

  public void setBinPath( final IPath path ) {
    check( path );
    binPath = path;
  }

  public void setOutputPath( final IPath path ) {
    check( path );
    outputPath = path;
  }

  private void check( final Object candidate ) {
    if( candidate == null ) {
      throw new IllegalArgumentException();
    }
  }
}
