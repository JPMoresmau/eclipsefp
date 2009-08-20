package net.sf.eclipsefp.haskell.core.internal.project;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.runtime.IPath;


public class ExecutableBuildTarget extends BuildTarget implements
    IExecutableBuildTarget {

  private final String main;

  public ExecutableBuildTarget( final IPath path, final String main ) {
    super( path );
    this.main = main;
  }

  public IPath getPlatformPath() {
    return ResourceUtil.executableName( getPath() );
  }

  public String getMain() {
    return main;
  }

}
