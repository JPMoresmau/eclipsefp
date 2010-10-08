package net.sf.eclipsefp.haskell.scion.client;

/** Scion server event types (codes).
 * 
 * These are conceptually split into two categories: configuration events and
 * server status changes. {@link #NEEDS_REBUILD NEEDS_REBUILD} through {@link #CONFIG_CATEGORY_END CONFIG_CATEGORY_END}
 * are configuration events.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public enum ScionServerEventType {
  /** Preferences have changed */
  /** Scion server needs building or rebuilding */
  NEEDS_REBUILD,
  /** Built-in scion-server is being built */
  BUILDING_BUILTIN,
  /** Built-in scion-server built successfully */
  BUILTIN_BUILD_OK,
  /** Built-in scion-server build failed. */
  BUILTIN_BUILD_FAILED,
  /** Scion server executable changed */
  EXECUTABLE_CHANGED,
  /** Scion server did not start */
  NOT_STARTED,
  /** Scion server started and running */
  VALID_RUNNING,
  /** Scion server stopped unexpectedly */
  UNEXPECTED_TERMINATION,
  /** Scion server terminated, quit command sent */
  TERMINATED;
  
  /** Configuration event predicate */
  public boolean configurationEvent() {
    return (this.compareTo(NEEDS_REBUILD) >= 0 && this.compareTo(EXECUTABLE_CHANGED) <= 0);
  }
  
  /** Server status event predicate */
  public boolean serverStatusEvent() {
    return !configurationEvent();
  }
}
