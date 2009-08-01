package net.sf.eclipsefp.haskell.core.project;

import org.eclipse.core.runtime.IPath;

/**
 * A build target: either an executable or a library.
 *
 * @author Thomas ten Cate
 */
public interface IBuildTarget {

  /**
   * Returns the path and name of the resulting file,
   * without extension, relative to the output directory.
   */
  public IPath getPath();

  /**
   * Returns the same as {@link #getPath()}, but with an added
   * extension on operating systems that require it.
   */
  public IPath getPlatformPath();

  /**
   * Sets the path and name of the resulting file,
   * without extension, relative to the output directory.
   */
  public void setPath(IPath path);

}
