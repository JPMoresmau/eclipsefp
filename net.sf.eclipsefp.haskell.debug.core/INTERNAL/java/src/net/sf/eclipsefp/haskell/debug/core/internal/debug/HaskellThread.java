package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;

/**
 * thread for debugging haskell. For the moment there is always one thread
 * @author jean-philippem
 *
 */
public class HaskellThread implements IThread {
  private HaskellBreakpoint breakpoint;
  private final HaskellDebugTarget target;
  private final HaskellStrackFrame frame=new HaskellStrackFrame( this );

  public HaskellThread(final HaskellDebugTarget target){
    this.target=target;
  }

  public IBreakpoint[] getBreakpoints() {
    if (breakpoint!=null){
      return new IBreakpoint[]{breakpoint};
    }
    return new IBreakpoint[0];
  }

  public void setBreakpoint( final HaskellBreakpoint breakpoint ) {
    this.breakpoint = breakpoint;
  }

  public String getName() {
    // TODO name of the command launched?
    return "Thread 1";
  }

  public int getPriority()  {
    return 0;
  }

  public IStackFrame[] getStackFrames()  {
    if (isSuspended()){
      return new IStackFrame[]{frame};
    }
    return new IStackFrame[0];

  }

  public IStackFrame getTopStackFrame()  {
    if (isSuspended()){
      return frame;
    }
    return null;
  }

  public boolean hasStackFrames() {
   return isSuspended();
  }

  public IDebugTarget getDebugTarget() {
    return target;
  }

  public ILaunch getLaunch() {
   return target.getLaunch();
  }

  public String getModelIdentifier() {
    return target.getModelIdentifier();
  }

  public Object getAdapter( final Class adapter ) {
    if (adapter.isAssignableFrom( HaskellThread.class )){
      return this;
    }
    return null;
  }

  public boolean canResume() {
    return isSuspended();
  }

  public boolean canSuspend() {
    return false;
  }

  public boolean isSuspended() {
    return target.isSuspended();
  }

  public void resume() throws DebugException {
   target.resume();
  }

  public void suspend() {
    target.suspend();

  }

  public boolean canStepInto() {
    return isSuspended();
  }

  public boolean canStepOver() {
    return false;
  }

  public boolean canStepReturn() {
    return false;
  }

  public boolean isStepping() {
    // TODO Auto-generated method stub
    return false;
  }

  public void stepInto() throws DebugException {
    target.sendRequest( GHCiSyntax.STEP_COMMAND, true );
    DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( frame, DebugEvent.CHANGE,DebugEvent.CONTENT )});

  }

  public void stepOver() {
    // NOOP

  }

  public void stepReturn() {
    // NOOP
  }

  public boolean canTerminate() {
    return target.canTerminate();
  }

  public boolean isTerminated() {
    return target.isTerminated();
  }

  public void terminate() throws DebugException {
    target.terminate();
  }

}
