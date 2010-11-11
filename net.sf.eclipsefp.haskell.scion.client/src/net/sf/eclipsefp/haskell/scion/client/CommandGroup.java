package net.sf.eclipsefp.haskell.scion.client;

import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;

/**
 * The base class for project and file command groups. It implements the primitive
 * scheduling rule, provides facilities for partial ordering of command groups when
 * neccessary.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public abstract class CommandGroup extends Job implements ISchedulingRule {
  /** Sequence number generator */
  private static AtomicInteger nextCmdSeq = new AtomicInteger();
  /** Sequence number used to maintain ordering between project and file command groups. */
  private int cmdSeqNo;
  
  /** The constructor, which assigns all project and file command groups have 
   * a sequence number.
   * 
   * @param jobName Job
   */
  public CommandGroup(String jobName) {
    super(jobName);
    cmdSeqNo = nextCmdSeq.incrementAndGet();
    setName( adornedName(jobName) );
  }
  
  /**
   * Formats the Job's name, includes the command group's sequence number for easier
   * rule scheduling debugging (should see command groups partially ordered by sequence
   * number when executed.)
   * 
   * @param jobName The Job's name
   * @return "Job Name [seqno]" string.
   */
  private String adornedName(final String jobName) {
    return jobName.concat(" [").concat(String.valueOf(cmdSeqNo)).concat("]");
  }

  /**
   * Predicate that enforces partial ordering of command groups.
   * 
   * @param rhs Right hand side of the comparison
   * @return True if this command group should be ordered before the right hand side.
   */
  public boolean orderBefore(CommandGroup rhs) {
    return (cmdSeqNo <= rhs.cmdSeqNo);
  }
  
  /**
   * ISchedulingRule contains() method: This rule contains another iff it is the same
   * scheduling rule.
   */
  public boolean contains(ISchedulingRule rule) {
    return (rule == this);
  }
  
  /**
   * ISchedulingRule isConflicting() method: Another Job conflicts iff it is the same
   * scheduling rule.
   */
  public boolean isConflicting(ISchedulingRule rule) {
    return (rule == this);
  }
  
  /**
   * Run the command group, anticipating a result.
   * 
   * @return The Job status after the command group completes.
   */
  public IStatus runGroupSynchronously() {
    schedule();
    
    while (getResult() == null) {
      try {
        join();
      } catch (InterruptedException irq) {
        // Keep going...
      }
    }
    
    return getResult();
  }
}
