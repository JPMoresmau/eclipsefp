package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import java.util.regex.Matcher;
import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IRegisterGroup;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IVariable;

/**
 * stack frame for debugging haskell. For the moment there is always one stack frame.
 * @author jean-philippem
 *
 */
public class HaskellStrackFrame implements IStackFrame {
  private final HaskellThread thread;
  private int lineNumber=-1;
  private String name="HaskellStrackFrame";
  private int charEnd=-1;
  private int charStart=-1;

  public HaskellStrackFrame(final HaskellThread thread){
    this.thread=thread;
  }

  public int getCharEnd() {
    return charEnd;
  }

  public int getCharStart() {
    return charStart;
  }

  public int getLineNumber() {
    return lineNumber;
    //return thread.getBreakpoints()[0].getMarker().getAttribute( IMarker.LINE_NUMBER, 0 );
  }

  public String getName() {
    return name;
   //return thread.getBreakpoints()[0].getMarker().getAttribute( IMarker.MESSAGE, "StackFrame" );
  }


  public void setLocation( final String location ) {
    name=location;

    Matcher m=GHCiSyntax.BREAKPOINT_LOCATION_PATTERN.matcher( location );
    if (m.matches()){
      //name= m.group(1) ;
      lineNumber=Integer.parseInt( m.group(2)) ;
      charStart=Integer.parseInt(m.group(3));
      charEnd=Integer.parseInt(m.group(4));
    } else {
      m=GHCiSyntax.BREAKPOINT_LOCATIONMULTILINE_PATTERN.matcher( location );
      lineNumber=Integer.parseInt( m.group(2)) ;
      charStart=Integer.parseInt(m.group(3));
      charEnd=-1;
    }
    DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( this, DebugEvent.CHANGE, DebugEvent.CONTENT )});

  }

  public IRegisterGroup[] getRegisterGroups()  {
   return new IRegisterGroup[0];
  }

  public IThread getThread() {
   return thread;
  }

  public IVariable[] getVariables() throws DebugException {
    return ((HaskellDebugTarget)getDebugTarget()).getVariables(this);
  }

  public boolean hasRegisterGroups() {
    return false;
  }

  public boolean hasVariables() {
   return true;
  }

  public IDebugTarget getDebugTarget() {
    return thread.getDebugTarget();
  }

  public ILaunch getLaunch() {
   return thread.getLaunch();
  }

  public String getModelIdentifier() {
    return thread.getModelIdentifier();
  }

  public Object getAdapter( final Class adapter ) {
    if (adapter.isAssignableFrom(this.getClass() )){
      return this;
    }
    return null;
  }

  public boolean canStepInto() {
    return thread.canStepInto();
  }

  public boolean canStepOver() {
    return thread.canStepOver();
  }

  public boolean canStepReturn() {
    return thread.canStepReturn();
  }

  public boolean isStepping() {
   return thread.isStepping();
  }

  public void stepInto() throws DebugException {
    thread.stepInto();

  }

  public void stepOver() throws DebugException {
   thread.stepOver();

  }

  public void stepReturn() throws DebugException {
    thread.stepReturn();

  }

  public boolean canResume() {
    return thread.canResume();
  }

  public boolean canSuspend() {
    return thread.canSuspend();
  }

  public boolean isSuspended() {
    return thread.isSuspended();
  }

  public void resume() throws DebugException {
    thread.resume();
  }

  public void suspend()  {
    thread.suspend();
  }

  public boolean canTerminate() {
   return thread.canTerminate();
  }

  public boolean isTerminated() {
   return thread.isTerminated();
  }

  public void terminate() throws DebugException {
    thread.terminate();

  }

}
