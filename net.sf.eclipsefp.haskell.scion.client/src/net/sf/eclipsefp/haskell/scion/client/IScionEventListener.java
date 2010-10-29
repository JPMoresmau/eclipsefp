package net.sf.eclipsefp.haskell.scion.client;

/** Interface for catching and processing scion server events */
public interface IScionEventListener {
  /** Process a scion server event.
   * 
   * @param ev The {@link ScionEvent} object that indicates the type of event that occurred.
   */
  public void processScionServerEvent(ScionEvent ev);
}
