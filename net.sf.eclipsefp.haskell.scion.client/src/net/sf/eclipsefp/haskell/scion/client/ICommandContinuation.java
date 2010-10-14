package net.sf.eclipsefp.haskell.scion.client;

/**
 * Interface class that runs a continuation after a scion-server command
 * completes.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public interface ICommandContinuation {
  /**
   * The method to be invoked after a ScionCommand job changes status and the job has completed
   * successfully.
   */
  public void commandContinuation();
}
