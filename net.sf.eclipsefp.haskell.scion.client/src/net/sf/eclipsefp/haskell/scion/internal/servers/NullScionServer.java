package net.sf.eclipsefp.haskell.scion.internal.servers;

public class NullScionServer extends ScionServer {
>>>>>>> 6df034b69955bae2cafbb287d1016ad796a2584b
  private final static class SingletonContainer {
    private static final NullScionServer theInstance = new NullScionServer();
  }

  private NullScionServer() {
    // Schultz: "I hear nossink!"
  }
  
  public static ScionServer getDefault() {
    return SingletonContainer.theInstance;
  }
}
