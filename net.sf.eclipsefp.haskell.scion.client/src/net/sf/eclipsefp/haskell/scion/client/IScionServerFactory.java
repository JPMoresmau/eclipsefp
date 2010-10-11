/**
 * 
 */
package net.sf.eclipsefp.haskell.scion.client;


/** Scion executable factory interface.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public interface IScionServerFactory {
  /** Create a new ScionExectable */
  IScionServer createScionServer();
}
