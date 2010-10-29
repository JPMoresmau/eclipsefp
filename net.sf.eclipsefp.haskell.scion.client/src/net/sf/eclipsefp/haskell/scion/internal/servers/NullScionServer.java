package net.sf.eclipsefp.haskell.scion.internal.servers;

import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

public class NullScionServer extends ScionServer {
  private final static class SingletonContainer {
    private static final NullScionServer theInstance = new NullScionServer();
  }

  private NullScionServer() {
    super();
  }
  
  public static ScionServer getDefault() {
    return SingletonContainer.theInstance;
  }
  
  @Override
  public boolean sendCommand(ScionCommand command) {
    return true;
  }
  
  @Override
  public boolean queueCommand(ScionCommand command) {
    return true;
  }
}
