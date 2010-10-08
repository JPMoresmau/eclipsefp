package net.sf.eclipsefp.haskell.scion.client;

import java.util.EventObject;

/**
 * Scion server events 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 *
 */
public class ScionServerEvent extends EventObject {
  /** Required serialization version UID; */
  private static final long serialVersionUID = 1905041239755384951L;
  /** Event type (one of the internal static constants */
  private ScionServerEventType evType;

  /** Event constructor */
  public ScionServerEvent(final IScionInstance source, final ScionServerEventType evType) {
    super(source);
    this.evType = evType;
  }
  
  /** Get the event type */
  public ScionServerEventType getEventType() {
    return evType;
  }
}
