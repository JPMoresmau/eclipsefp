package net.sf.eclipsefp.haskell.core.internal.project;

import net.sf.eclipsefp.haskell.core.project.IBuildTarget;
import org.eclipse.core.runtime.IPath;

/**
 * The canonical implementation of {@link IBuildTarget}.
 *
 * @author Thomas ten Cate
 */
public abstract class BuildTarget implements IBuildTarget {

  private IPath path;

  public BuildTarget(final IPath path) {
    setPath(path);
  }

  public IPath getPath() {
    return path;
  }

  public void setPath( final IPath path ) {
    this.path = path;
  }

}
