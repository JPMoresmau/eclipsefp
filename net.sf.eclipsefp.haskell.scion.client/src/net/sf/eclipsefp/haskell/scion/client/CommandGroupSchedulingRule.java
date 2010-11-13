package net.sf.eclipsefp.haskell.scion.client;

import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.core.runtime.jobs.ISchedulingRule;

public class CommandGroupSchedulingRule implements ISchedulingRule {
  /** Sequence number generator */
  private static AtomicInteger nextCmdSeq = new AtomicInteger();
  /** The command group sequence number for this rule */
  private int cmdGroupSeqno;
  
  public CommandGroupSchedulingRule() {
    this.cmdGroupSeqno = nextCmdSeq.incrementAndGet();;
  }

  /**
   * Accessor for the command group sequence number.
   */
  public int getCommandGroupSeqno() {
    return cmdGroupSeqno;
  }
  
  /**
   * Predicate that enforces partial ordering of command groups.
   * 
   * @param rhs Right hand side of the comparison
   * @return True if this command group should be ordered before the right hand side.
   */
  public boolean orderBefore(CommandGroupSchedulingRule rhs) {
    return (cmdGroupSeqno <= rhs.cmdGroupSeqno);
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

}
