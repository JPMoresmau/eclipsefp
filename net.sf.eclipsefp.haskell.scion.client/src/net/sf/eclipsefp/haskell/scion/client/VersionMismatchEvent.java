package net.sf.eclipsefp.haskell.scion.client;

import net.sf.eclipsefp.haskell.scion.internal.servers.ScionServer;

public class VersionMismatchEvent extends ScionEvent {
  /**
   * Required version identifier.
   */
  private static final long serialVersionUID = 4213560738304941334L;
  /** The expected version number that the server should have sent us. */
  private static final String expectedVersion = ScionServer.PROTOCOL_VERSION;
  /** The received version number we got... */
  private final String receivedVersion;

  public VersionMismatchEvent(ScionInstance source, ScionServer server, String received) {
    super(source, server, ScionEventType.PROTOCOL_VERSION_MISMATCH);
    this.receivedVersion = received;
  }
  
  public final String getExpectedVersion() {
    return expectedVersion;
  }
  
  public final String getReceivedVersion() {
    return receivedVersion;
  }
}
