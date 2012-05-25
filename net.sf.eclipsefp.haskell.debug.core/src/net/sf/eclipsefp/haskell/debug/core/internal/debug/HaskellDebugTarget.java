package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IVariable;

/**
 * debug target for haskell interactive launch
 * @author JP Moresmau
 *
 */
public class HaskellDebugTarget extends HaskellDebugElement implements IDebugTarget,IStreamListener {
  // associated system process (VM)
  private final IProcess fProcess;

  // containing launch object
  private final ILaunch fLaunch;

  // program name
  private String fName;

  private final StringBuilder response=new StringBuilder();
  private final Map<IBreakpoint,Integer> breakpointIds=new IdentityHashMap<IBreakpoint, Integer>();
  private final Map<String,HaskellBreakpoint> breakpointNames=new HashMap<String,HaskellBreakpoint>();


  private boolean connected=true;
  private boolean atEnd=false;

  private final HaskellThread thread=new HaskellThread( this );

  public HaskellDebugTarget(final ILaunch launch, final IProcess process){
    setTarget( this );
    this.fLaunch=launch;
    this.fProcess=process;
    this.fProcess.getStreamsProxy().getOutputStreamMonitor().addListener( this );
    DebugPlugin.getDefault().getBreakpointManager().addBreakpointListener(this);

  }

  @Override
  public String getName(){
    if (fName == null) {
       fName = getLaunch().getLaunchConfiguration().getName();
    }
    return fName;
  }

  @Override
  public IProcess getProcess() {
    return fProcess;
  }

  @Override
  public IThread[] getThreads(){
   return new IThread[]{thread};
  }

  @Override
  public boolean hasThreads() {
    return true;
  }

  @Override
  public boolean supportsBreakpoint( final IBreakpoint breakpoint ) {
    if (breakpoint.getModelIdentifier().equals(HaskellDebugCore.ID_HASKELL_DEBUG_MODEL)) {

// see http://sourceforge.net/projects/eclipsefp/forums/forum/371922/topic/5091430, we don't want to reduce only to our project
        IMarker marker = breakpoint.getMarker();
        if (marker != null) {
          try {
            String project = getLaunch().getLaunchConfiguration().getAttribute(ILaunchAttributes.PROJECT_NAME, (String)null);
            IProject launchProject=marker.getResource().getProject().getWorkspace().getRoot().getProject( project );
            if (launchProject!=null){
              if (launchProject.equals(marker.getResource().getProject())){
                return true;
              }
              for( IProject p: launchProject.getReferencedProjects() ) {
                if (p.equals(marker.getResource().getProject())){
                  return true;
                }
              }
            }
          } catch (CoreException e) {
            HaskellCorePlugin.log( e );
          }
        }
//      try {
//        String project = getLaunch().getLaunchConfiguration().getAttribute(ILaunchAttributes.PROJECT_NAME, (String)null);
//        if (project != null) {
//          IMarker marker = breakpoint.getMarker();
//          if (marker != null) {
//            return project.equals(marker.getResource().getProject().getName());
//          }
//        }
//      } catch (CoreException e) {
//        HaskellCorePlugin.log( e );
//      }

    }
    return false;
  }

  @Override
  public ILaunch getLaunch() {
    return fLaunch;
  }


  @Override
  public boolean canTerminate() {
   return fProcess.canTerminate();
  }

  @Override
  public boolean isTerminated() {
   return fProcess.isTerminated();
  }

  protected synchronized void sendRequest(final String command,final boolean wait)throws DebugException{
    synchronized( response ) {
      response.setLength( 0 );
      atEnd=false;
    }
    try {

      fProcess.getStreamsProxy().write(command);
      fProcess.getStreamsProxy().write(PlatformUtil.NL);
      if (wait){
        waitForPrompt();
      }

    } catch (IOException ioe){
      throw new DebugException(new Status(IStatus.ERROR,HaskellDebugCore.getPluginId(),ioe.getLocalizedMessage(),ioe));
    }
  }

  private synchronized void waitForPrompt(){
    long timeout=10; // seconds
    long t0=System.currentTimeMillis();
    try {
      while(!atEnd && System.currentTimeMillis()-t0<(timeout * 1000)){
        wait(100);
      }
    } catch (InterruptedException ie){
      ie.printStackTrace();
    }
  }

  @Override
  public void terminate() throws DebugException {
    sendRequest( GHCiSyntax.QUIT_COMMAND,false );
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
    return thread.getBreakpoints().length>0 || thread.getStopLocation()!=null;
  }

  @Override
  public void resume() throws DebugException {
    thread.setBreakpoint( null );
    thread.setStopLocation( null );
    DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( thread, DebugEvent.RESUME )});
    sendRequest( GHCiSyntax.CONTINUE_COMMAND, false );

  }

  @Override
  public void suspend()  {
    // NOOP
  }

  @Override
  public synchronized void breakpointAdded( final IBreakpoint breakpoint ) {
    if (supportsBreakpoint(breakpoint)) {
      try {
        if (breakpoint.isEnabled()) {
            HaskellBreakpoint hb=(HaskellBreakpoint)breakpoint;
         // TODO take out GHCi specific
            IPath path=ResourceUtil.getSourceFolderRelativeName( hb.getMarker().getResource() );
            String module=ResourceUtil.getModuleName( path.toPortableString() );
            sendRequest(GHCiSyntax.setBreakpointCommand(module.replace('/','.'),(hb.getLineNumber()) ),true);
            String s=response.toString();
            /*int ix=s.indexOf( "Breakpoint " );
            ix+="Breakpoint ".length();
            int ix2=s.indexOf( " activated ",ix );
            Integer id=Integer.valueOf( s.substring( ix,ix2 ) );
            int ix3=s.indexOf( ResourceUtil.NL,ix2 );*/
            Matcher m=GHCiSyntax.BREAKPOINT_SET_PATTERN.matcher( s );
            boolean found=m.find();
            if (!found){
              try {
                  wait(100);
              } catch (InterruptedException ie){
                ie.printStackTrace();
              }
              s=response.toString();
              m=GHCiSyntax.BREAKPOINT_SET_PATTERN.matcher( s );
              found=m.find();
            }
            if (found){
              Integer id=Integer.valueOf(m.group( 1 ));
              String name=m.group( 2 );
              breakpointIds.put( breakpoint, id );
              breakpointNames.put(name,hb);

            } else {
              System.err.println(s);
            }
         }
      } catch (CoreException e) {
        HaskellCorePlugin.log( e );
      }
    }

  }

  @Override
  public void breakpointChanged( final IBreakpoint breakpoint, final IMarkerDelta delta ) {
    if (supportsBreakpoint(breakpoint)) {
      try {
        if (breakpoint.isEnabled()) {
          breakpointAdded(breakpoint);
        } else {
          breakpointRemoved(breakpoint, null);
        }
      } catch (CoreException e) {
        HaskellCorePlugin.log( e );
      }
    }
  }

  @Override
  public void breakpointRemoved( final IBreakpoint breakpoint, final IMarkerDelta delta ) {
    // TODO take out GHCi specific
    Integer id=breakpointIds.get(breakpoint);
    if (id!=null){
      try {
        sendRequest(GHCiSyntax.deleteBreakpointCommand( id.intValue()),true);
      } catch (CoreException e) {
        HaskellCorePlugin.log( e );
      }
    }
  }

  @Override
  public boolean canDisconnect() {
    return connected;
  }

  @Override
  public void disconnect() throws DebugException {
    connected=false;
    try {
   // TODO take out GHCi specific
      sendRequest(GHCiSyntax.DELETE_ALL_BREAKPOINTS_COMMAND,true);
    } catch (CoreException e) {
      throw new DebugException(new Status(IStatus.ERROR,HaskellDebugCore.getPluginId(),e.getLocalizedMessage(),e));

    }

  }

  @Override
  public boolean isDisconnected() {
      return !connected;
  }

  @Override
  public IMemoryBlock getMemoryBlock( final long startAddress, final long length ) {
    return null;
  }

  @Override
  public boolean supportsStorageRetrieval() {
    return false;
  }

  public void start() throws DebugException{

    //waitForPrompt();
    IBreakpoint[] breakpoints = DebugPlugin.getDefault().getBreakpointManager().getBreakpoints(HaskellDebugCore.ID_HASKELL_DEBUG_MODEL);
    for (int i = 0; i < breakpoints.length; i++) {
      breakpointAdded(breakpoints[i]);
    }
    IPreferencesService service = Platform.getPreferencesService();
    if (service.getBoolean(HaskellCorePlugin.getPluginId(), ICorePreferenceNames.DEBUG_PRINT_WITH_SHOW ,true,null)){
      sendRequest( GHCiSyntax.SET_PRINT_WITH_SHOW_COMMAND, false );
    }
    if (service.getBoolean(HaskellCorePlugin.getPluginId(), ICorePreferenceNames.DEBUG_BREAK_ON_ERROR,false,null )){
      sendRequest( GHCiSyntax.SET_BREAK_ON_ERROR_COMMAND, false );
    }
    if (service.getBoolean(HaskellCorePlugin.getPluginId(), ICorePreferenceNames.DEBUG_BREAK_ON_EXCEPTION,false,null )){
      sendRequest( GHCiSyntax.SET_BREAK_ON_EXCEPTION_COMMAND, false );
    }
  }

  //boolean runContext=true;
  @Override
  public synchronized void streamAppended( final String text, final IStreamMonitor monitor ) {
    //boolean needContext=false;
    synchronized( response ) {
     atEnd=false;
     response.append(text);
     atEnd=text.endsWith( GHCiSyntax.PROMPT_END);
     if (atEnd){
       if (thread.isSuspended()){
         Matcher m2=GHCiSyntax.CONTEXT_PATTERN.matcher( response.toString() );
         if (m2.find()){
           String name=m2.group( 1 );
           thread.setName( name );
           response.setLength( 0 );
           DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( thread, DebugEvent.CHANGE,DebugEvent.STATE )});
           notify();
           return;
         }
       }
       Matcher m=GHCiSyntax.BREAKPOINT_STOP_PATTERN.matcher( response.toString() );
       if (m.find()){
         String location=m.group( 1 );
         HaskellBreakpoint hb=breakpointNames.get(location );
         boolean wasSuspended=thread.isSuspended();
         if (hb!=null){
           thread.setBreakpoint( hb );
         } else {
           thread.setStopLocation( location );
         }
         response.setLength( 0 );
         if (thread.isSuspended()){
           HaskellStrackFrame hsf=(HaskellStrackFrame)thread.getTopStackFrame();
           hsf.setLocation( location );
           //needContext=true;
           if (!wasSuspended){
             try {
               //runContext=false;
               sendRequest( GHCiSyntax.SHOW_CONTEXT_COMMAND, false );

             } catch (DebugException de){
               HaskellCorePlugin.log( de );
             }
           }
         }

         DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( thread, DebugEvent.SUSPEND,hb!=null?DebugEvent.BREAKPOINT:DebugEvent.UNSPECIFIED )});


       } else {
         m=GHCiSyntax.BREAKPOINT_NOT.matcher( response.toString() );
         if (m.find()){
           thread.setBreakpoint(null);
           DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( thread, DebugEvent.RESUME )});
           response.setLength( 0 );
         }
       }
     }
     notify();
   }
    /*if (needContext)  {
      try {
        //runContext=false;
        sendRequest( GHCiSyntax.SHOW_CONTEXT_COMMAND, false );

      } catch (DebugException de){
        HaskellCorePlugin.log( de );
      } finally {
        //runContext=true;
      }
    }*/

  }

  public synchronized IVariable[] getVariables( final HaskellStrackFrame frame ) throws DebugException {
    sendRequest( GHCiSyntax.SHOW_BINDINGS_COMMAND, true );
    String s=getResultWithoutPrompt();
    BufferedReader br=new BufferedReader(new StringReader( s ));
    try {
      List<IVariable> ret=new ArrayList<IVariable>();
      String line=br.readLine();
      StringBuilder sb=new StringBuilder();
      while (line!=null){

        if (line.indexOf( GHCiSyntax.TYPEOF )>-1 && sb.length()>0){
          ret.add( new HaskellVariable( sb.toString(), frame ) );
          sb.setLength( 0 );
        }
        if (sb.length()>0){
          sb.append(PlatformUtil.NL);
        }
        sb.append( line );
        line=br.readLine();
      }
      if (sb.length()>0){
        ret.add( new HaskellVariable( sb.toString(), frame ) );
      }
      return ret.toArray( new IVariable[ret.size()] );
    } catch (IOException ioe){
      throw new DebugException(new Status(IStatus.ERROR,HaskellDebugCore.getPluginId(),ioe.getLocalizedMessage(),ioe));
    }
  }

  public void forceVariable(final HaskellVariable var)throws DebugException{
    sendRequest( GHCiSyntax.forceVariableCommand( var.getName() ), true );
    DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( var.getFrame(), DebugEvent.CHANGE,DebugEvent.CONTENT)});

    /*String s=response.toString();
    BufferedReader br=new BufferedReader(new StringReader( s ));
    try {
      String line=br.readLine();
      while (line!=null){
        if (line.indexOf( GHCiSyntax.TYPEOF )>-1){
          return line;
        }
        line=br.readLine();
      }
      return null;
    } catch (IOException ioe){
      throw new DebugException(new Status(IStatus.ERROR,HaskellDebugCore.getPluginId(),ioe.getLocalizedMessage(),ioe));
    }*/
  }

  /**
   * evaluate an arbitrary expression
   * @param expression the expression
   * @return the value and its type
   * @throws DebugException
   */
  public synchronized HaskellValue evaluate(final String expression)throws DebugException{
    // get rid of any previous "it" in case of evaluation error
    sendRequest(GHCiSyntax.UNIT,true);
    sendRequest(expression,true);
    String val=getResultWithoutPrompt();
    sendRequest(GHCiSyntax.TYPE_LAST_RESULT_COMMAND,true);
    String type=getResultWithoutPrompt();
    int ix=type.indexOf( GHCiSyntax.TYPEOF );
    if (ix>-1){
      type=type.substring( ix+GHCiSyntax.TYPEOF.length() ).trim();
    } else {
      type=""; //$NON-NLS-1$
    }
    return new HaskellValue(this,type,val);
  }

  private String getResultWithoutPrompt(){
    String s=response.toString();
    int ix=s.lastIndexOf( PlatformUtil.NL );
    if(ix>-1){
      s=s.substring(0,ix).trim();
    }
    return s;
  }
}
