package net.sf.eclipsefp.haskell.core.internal.project;

import net.sf.eclipsefp.haskell.core.project.IBuildTarget;

/**
 * A build target that is an executable file.
 *
 * @author Thomas ten Cate
 */
public interface IExecutableBuildTarget extends IBuildTarget {

  /**
   * Returns the fully qualified name of the main function,
   * for example "Main.main".
   */
  public String getMain();

  /**
   * Sets the name of the main function.
   */
  public void setMain(String main);

}
