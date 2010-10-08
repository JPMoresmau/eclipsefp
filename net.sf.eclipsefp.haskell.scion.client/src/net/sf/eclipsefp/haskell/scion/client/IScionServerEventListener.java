package net.sf.eclipsefp.haskell.scion.client;

/**
 * Abstract class for all scion server events
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public interface IScionServerEventListener {
  public void processScionServerEvent(ScionServerEvent ev);
}
