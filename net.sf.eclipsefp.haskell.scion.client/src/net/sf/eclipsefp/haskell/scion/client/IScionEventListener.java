package net.sf.eclipsefp.haskell.scion.client;

/** Interface for catching and processing scion server events */
public interface IScionEventListener {
  /** Process a scion server event */
  public void processScionServerEvent(ScionEvent ev);
}
