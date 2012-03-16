package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;

/**
 * thread for debugging haskell. For the moment there is always one thread
 * @author JP Moresmau
 *
 */
public class HaskellThread extends HaskellDebugElement implements IThread {
  private HaskellBreakpoint breakpoint;
  private String stopLocation;
  private final HaskellStrackFrame frame=new HaskellStrackFrame( this );
  private String name=CoreTexts.thread_default_name;

  public HaskellThread(final HaskellDebugTarget target){
    super( target );
  }

  @Override
  public IBreakpoint[] getBreakpoints() {
    if (breakpoint!=null){
      return new IBreakpoint[]{breakpoint};
    }
    return new IBreakpoint[0];
  }

  public void setBreakpoint( final HaskellBreakpoint breakpoint ) {
    this.breakpoint = breakpoint;
  }

  @Override
  public String getName() {
    return name;
  }


  public void setName( final String name ) {
    this.name = name;
  }

  @Override
  public int getPriority()  {
    return 0;
  }

  @Override
  public IStackFrame[] getStackFrames()  {
    if (isSuspended()){
      return new IStackFrame[]{frame};
    }
    return new IStackFrame[0];

  }

  @Override
  public IStackFrame getTopStackFrame()  {
    if (isSuspended()){
      return frame;
    }
    return null;
  }

  @Override
  public boolean hasStackFrames() {
   return isSuspended();
  }

   @Override
  public boolean canResume() {
    return isSuspended();
  }

  @Override
  public boolean canSuspend() {
    return false;
  }

  @Override
  public boolean isSuspended() {
    return target.isSuspended();
  }

  @Override
  public void resume() throws DebugException {
   target.resume();
  }

  @Override
  public void suspend()  {
    getDebugTarget().suspend();

  }

  @Override
  public boolean canStepInto() {
    return isSuspended();
  }

  @Override
  public boolean canStepOver() {
    return false;
  }

  @Override
  public boolean canStepReturn() {
    return false;
  }

  @Override
  public boolean isStepping() {
    return false;
  }

  @Override
  public void stepInto() throws DebugException {
    target.sendRequest( GHCiSyntax.STEP_COMMAND, true );
    DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( frame, DebugEvent.CHANGE,DebugEvent.CONTENT )});

  }

  @Override
  public void stepOver() {
    // NOOP

  }

  @Override
  public void stepReturn() {
    // NOOP
  }

  @Override
  public boolean canTerminate() {
    return target.canTerminate();
  }

  @Override
  public boolean isTerminated() {
    return target.isTerminated();
  }

  @Override
  public void terminate() throws DebugException {
    target.terminate();
  }


  public String getStopLocation() {
    return stopLocation;
  }


  public void setStopLocation( final String stopLocation ) {
    this.stopLocation = stopLocation;
  }

}
