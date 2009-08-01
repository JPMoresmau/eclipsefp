package net.sf.eclipsefp.haskell.core.internal.project;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.runtime.IPath;


public class ExecutableBuildTarget extends BuildTarget implements
    IExecutableBuildTarget {

  private String main = "Main.main"; //$NON-NLS-1$

  public ExecutableBuildTarget( final IPath path ) {
    super( path );
  }

  public IPath getPlatformPath() {
    return ResourceUtil.executableName( getPath() );
  }

  public String getMain() {
    return main;
  }

  public void setMain( final String main ) {
    this.main = main;
  }

}
