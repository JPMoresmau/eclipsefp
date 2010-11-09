package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.core.runtime.jobs.Job;

public abstract class CommandGroup extends Job {
  /** Sequence number generator */
  private static AtomicInteger nextCmdSeq = new AtomicInteger();
  /** Sequence number used to maintain ordering between project and file command groups. */
  private int cmdSeqNo;
  
  /** The constructor, which only exists to ensure that all project and file command groups have 
   * a sequence number.
   * 
   * @param jobName Job
   */
  public CommandGroup(String jobName) {
    super(jobName);
    cmdSeqNo = nextCmdSeq.incrementAndGet();
  }
  
  public int getCommandSequence() {
    return cmdSeqNo;
  }
}
