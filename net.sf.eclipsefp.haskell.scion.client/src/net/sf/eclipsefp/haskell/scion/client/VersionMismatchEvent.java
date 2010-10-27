package net.sf.eclipsefp.haskell.scion.client;

import net.sf.eclipsefp.haskell.scion.internal.servers.ScionServer;

public class VersionMismatchEvent extends ScionEvent {
  /**
   * Required version identifier.
   */
  private static final long serialVersionUID = 4213560738304941334L;
  /** The expected version number that the server should have sent us. */
  private static final int expectedVersion = ScionServer.WIRE_PROTOCOL_VERSION;
  /** The received version number we got... */
  private final int receivedVersion;

  public VersionMismatchEvent(ScionInstance source, ScionServer server, int received) {
    super(source, server, ScionEventType.PROTOCOL_VERSION_MISMATCH);
    this.receivedVersion = received;
  }
  
  public final int getExpectedVersion() {
    return expectedVersion;
  }
  
  public final int getReceivedVersion() {
    return receivedVersion;
  }
}
