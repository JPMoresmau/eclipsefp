package net.sf.eclipsefp.haskell.scion.client;

import java.io.Writer;

import org.eclipse.core.resources.IProject;

import net.sf.eclipsefp.haskell.scion.internal.client.NullScionServer;

/**
 * The null scion server factory.
 * 
 * @author B. Scott Michel
 */
public class NullScionServerFactory implements IScionServerFactory {
  /** Container class for the {@link NullScionServerFactory NullScionServerFactory} singleton
   * instance.
   */
  private final static class SingletonContainer {
    private final static NullScionServerFactory theInstance = new NullScionServerFactory();
  }
  
  /** Default constructor. This is hidden so preserve singleton semantics. */
  private NullScionServerFactory() {
    // NOP
  }
  
  /** Get the singleton factory instance */
  public final static NullScionServerFactory getDefault() {
    return SingletonContainer.theInstance;
  }

  /** Create a new NullScionServer. In reality, this just returns another reference
   * to the {@link NullScionServer NullScionServer}'s singleton. */
  public IScionServer createScionServer(final IProject project, final Writer outStream) {
    return NullScionServer.getDefault();
  }
}
