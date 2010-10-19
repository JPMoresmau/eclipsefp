package net.sf.eclipsefp.haskell.scion.client;

/** Interface for catching and processing scion server events */
public interface IScionServerEventListener {
  /** Process a scion server event */
  public void processScionServerEvent(ScionServerEvent ev);
}
