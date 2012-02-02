package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import java.util.regex.Matcher;
import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IRegisterGroup;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IVariable;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * stack frame for debugging haskell. For the moment there is always one stack frame.
 * @author JP Moresmau
 *
 */
public class HaskellStrackFrame extends HaskellDebugElement implements IStackFrame {
  private final HaskellThread thread;
  private int lineNumber=-1;
  private String name="HaskellStrackFrame"; //$NON-NLS-1$
  /**
   * relative file name
   */
  private String fileName;
  /**
   * full file name
   */
  private String unprocessedFileName;
  private int charEnd=-1;
  private int charStart=-1;

  public HaskellStrackFrame(final HaskellThread thread){
    super(thread.getDebugTarget());
    this.thread=thread;
  }

  public int getCharEnd() {
    // return -1
    return this.charEnd;
  }

  public int getCharStart() {
    // return -1;
    return this.charStart;
  }

  public int getLineNumber() {
    return lineNumber;
  }

  public String getName() {
    return name;
  }


  public void setLocation( final String location ) {
    name=location;
    lineNumber=-1;
    fileName=null;
    unprocessedFileName=location;
    int endLineNumber = -1, tmpCharStart = -1, tmpCharEnd = -1;
    Matcher m=GHCiSyntax.BREAKPOINT_LOCATION_PATTERN.matcher( location );
    if (m.matches()){
      //name= m.group(1) ;
      endLineNumber=lineNumber=Integer.parseInt( m.group(2)) ;
      unprocessedFileName=m.group( 1 );

      tmpCharStart=Integer.parseInt(m.group(3));
      tmpCharEnd=Integer.parseInt(m.group(4));
    } else {
      m=GHCiSyntax.BREAKPOINT_LOCATIONMULTILINE_PATTERN.matcher( location );
      if (m.matches()){
        lineNumber=Integer.parseInt( m.group(2)) ;
        endLineNumber=Integer.parseInt( m.group(4) );
        unprocessedFileName=m.group( 1 );
        tmpCharStart=Integer.parseInt(m.group(3));
        tmpCharEnd=Integer.parseInt(m.group(5));
      } else {
        // It means there was an error
        endLineNumber = -1;
      }
    }
    // Compute real character positions
    if (endLineNumber == -1) {
      // If there was an error
      lineNumber = -1;
      charStart = -1;
      charEnd = -1;
    } else {
      try {
        // Get file name
        String fname=unprocessedFileName;
        IProject p = getProject();
        String projectLocation=p.getLocation().toOSString();
        if (fname.startsWith( projectLocation )){
          fname=fname.substring( projectLocation.length()+1 );
        }
        IFile f= p.getFile( fname );
        IDocumentProvider prov=new TextFileDocumentProvider();
        prov.connect( f );
        try {
          IDocument doc=prov.getDocument( f );

          //InputStream st = p.getFile( fname ).getContents();
          // Get document info
          //IDocument d = new Document( ResourceUtil.readStream( st ) );
          //st.close();
          int initLineOffset = doc.getLineOffset( lineNumber - 1 );
          int endLineOffset = doc.getLineOffset( endLineNumber - 1  );
          charStart = initLineOffset + tmpCharStart - 1;
          charEnd = endLineOffset + tmpCharEnd;
        } finally {
          prov.disconnect( f );
        }
      } catch (Exception e) {
        lineNumber = -1;
        charStart = -1;
        charEnd = -1;
      }
    }

    DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[]{new DebugEvent( this, DebugEvent.CHANGE, DebugEvent.CONTENT )});
  }


  public String getFileName() throws CoreException{
    if (fileName==null){
      fileName=unprocessedFileName;
      IProject p = getProject();
      String projectLocation=p.getLocation().toOSString();
      // return file relative to project, since we may have the same file in different folders (Main.hs)
      if (fileName.startsWith( projectLocation )){
        fileName=fileName.substring( projectLocation.length()+1 );
      }
    }
    return fileName;
  }

  public IProject getProject() throws CoreException {
    String projectName=getLaunch().getLaunchConfiguration().getAttribute( ILaunchAttributes.PROJECT_NAME ,(String)null);
    if (projectName!=null){
      IProject p=ResourcesPlugin.getWorkspace().getRoot().getProject( projectName );
      return p;
    }
    return null;
  }

  public IRegisterGroup[] getRegisterGroups()  {
   return new IRegisterGroup[0];
  }

  public IThread getThread() {
   return thread;
  }

  public IVariable[] getVariables() throws DebugException {
    return getDebugTarget().getVariables(this);
  }

  public boolean hasRegisterGroups() {
    return false;
  }

  public boolean hasVariables() {
   return true;
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

  public void stepOver() {
   thread.stepOver();

  }

  public void stepReturn()  {
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
