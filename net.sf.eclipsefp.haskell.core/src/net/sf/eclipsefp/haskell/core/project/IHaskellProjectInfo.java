package net.sf.eclipsefp.haskell.core.project;

import java.util.Set;
import org.eclipse.core.runtime.IPath;

/**
 * Represents information about a Haskell project:
 * source paths, output paths, target binaries, etcetera.
 * This information is either stored in a Cabal file,
 * or in the .hsproject file.
 *
 * @author Thomas ten Cate
 *
 * TODO removal of source paths and targets
 */
public interface IHaskellProjectInfo {

  /**
   * Returns the set of source folders, relative to the project root.
   */
  public Set<IPath> getSourcePaths();

  /**
   * Add a source folder, relative to the project root, to the set of source folders.
   */
  public void addSourcePath(IPath path);

  /**
   * Returns the set of build targets.
   */
  public Set<IBuildTarget> getTargets();

  /**
   * Adds a build target.
   */
  public void addTarget(IBuildTarget target);

  /**
   * Returns the path to which build target binaries should be written,
   * relative to the project root.
   */
  public IPath getOutputPath();

  /**
   * Sets the path to which intermediate build files should be written,
   * relative to the project root.
   */
  public void setOutputPath(IPath path);

  /**
   * Returns the path to which intermediate build files should be written,
   * relative to the project root.
   */
  public IPath getBuildPath();

  /**
   * Sets the path to which intermediate build files should be written,
   * relative to the project root.
   */
  public void setBuildPath(IPath path);

}
