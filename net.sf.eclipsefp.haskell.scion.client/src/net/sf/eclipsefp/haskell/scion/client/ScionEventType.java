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
   * 
   * Tell listeners that the initial {@link ScionInstance} startup has completed.
   * 
   * @note This is a contrived server event: the initial {@link ScionInstance#buildProject(boolean, boolean) buildProject}
   * that ScionInstance.start kicks off is a heavyweight process; this is scheduled as an Eclipse Job to prevent startup
   * from taking an excessive amount of time. However, once buildProject() completes, editors need to refresh and reload
   * the current file. This event is a signal to interested listeners, primarily HaskellEditor, that buildProject() has
   * finished.
   */
  BUILD_PROJECT_COMPLETED
}
