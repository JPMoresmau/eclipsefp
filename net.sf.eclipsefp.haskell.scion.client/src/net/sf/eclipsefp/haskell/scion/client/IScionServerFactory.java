/**
 * 
 */
package net.sf.eclipsefp.haskell.scion.client;

import java.io.Writer;

import org.eclipse.core.resources.IProject;


/** Scion executable factory interface.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public interface IScionServerFactory {
  /** Create a new ScionExectable 
   * @param project The associated project, which identifies the working directory for the server
   * @param outStream TODO
   */
  public IScionServer createScionServer(final IProject project, final Writer outStream);
}
