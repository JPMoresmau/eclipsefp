/**
 * (c) 2010, B. Scott Michel
 */
package net.sf.eclipsefp.haskell.scion.client;

/**
 * Scion event types. These events communicate various Scion client interactions
 * and execution environment changes to interested listeners.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public enum ScionEventType {
  /**
   * Server started successfully
   */
  SERVER_STARTED,
  /**
   * The executable changed, usually due to a property change in the EclipseFP
   * preferences pane or as the result of the built-in server compilation
   * process.
   */
  EXECUTABLE_CHANGED,
  /**
   * The underlying scion-server process just terminated abnormally.
   */
  ABNORMAL_TERMINATION,
  /**
   * Inform listeners that a buildProject() has completed.
   */
  BUILD_PROJECT_COMPLETED,
  /**
   * Inform listeners that the received scion-server protocol version does not match expected protocol
   * version number.
   */
  PROTOCOL_VERSION_MISMATCH
}
