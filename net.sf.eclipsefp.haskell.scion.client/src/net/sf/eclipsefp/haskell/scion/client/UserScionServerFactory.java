package net.sf.eclipsefp.haskell.scion.client;

import org.eclipse.core.runtime.IPath;

public class UserScionServerFactory implements IScionServerFactory {

  private IPath userExecutablePath;
  
  public UserScionServerFactory(final IPath userExecutablePath) {
    this.userExecutablePath = userExecutablePath;
  }
  
  public IScionServer createScionServer() {
    // TODO Auto-generated method stub
    return null;
  }

}
