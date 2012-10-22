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

  private boolean hasVariables=false;

  public HaskellStrackFrame(final HaskellThread thread){
    super(thread.getDebugTarget());
    this.thread=thread;
  }

  @Override
  public int getCharEnd() {
    // return -1
    return this.charEnd;
  }

  @Override
  public int getCharStart() {
    // return -1;
    return this.charStart;
  }

  @Override
  public int getLineNumber() {
    return lineNumber;
  }

  @Override
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
        hasVariables=true;
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

  public void setHistoryLocation( final String location ) {
    name=null;
    lineNumber=-1;
    fileName=null;
    unprocessedFileName=location;
    int endLineNumber = -1, tmpCharStart = -1, tmpCharEnd = -1, idx=-1;

    Matcher m=GHCiSyntax.HIST_LOCATION_PATTERN.matcher( location );
    if (m.matches()){
      //name= m.group(1) ;
      idx=Integer.parseInt( m.group(1));
      endLineNumber=lineNumber=Integer.parseInt( m.group(4)) ;
      unprocessedFileName=m.group( 3 );

      tmpCharStart=Integer.parseInt(m.group(5));
      tmpCharEnd=Integer.parseInt(m.group(6));
    } else {
      m=GHCiSyntax.HIST_LOCATIONMULTILINE_PATTERN.matcher( location );
      if (m.matches()){
        idx=Integer.parseInt( m.group(1));
        //name= m.group(1) ;
        lineNumber=Integer.parseInt( m.group(4)) ;
        endLineNumber=Integer.parseInt( m.group(6) );
        unprocessedFileName=m.group( 3 );
        tmpCharStart=Integer.parseInt(m.group(5));
        tmpCharEnd=Integer.parseInt(m.group(6));
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
        hasVariables=idx==1;
        name=location;
        int ix=name.indexOf( ':' );
        name=name.substring( ix+1 ).trim();
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


  /**
   * @return the hasVariables
   */
  @Override
  public boolean hasVariables() {
    return hasVariables;
  }

  public String getFileName() throws CoreException{
    if (fileName==null){
      fileName=unprocessedFileName;
      IProject p = getProject();
      String projectLocation=p.getLocation().toOSString();
      // return file relative to project, since we may have the same file in different folders (Main.hs)
      if (fileName.startsWith( projectLocation )){
        fileName=fileName.substring( projectLocation.length()+1 );
        return fileName;
      }
      // see http://sourceforge.net/projects/eclipsefp/forums/forum/371922/topic/5091430, we don't want to reduce only to our project
      // not found in my project, look in all reference projects
      for (IProject p1:getProject().getReferencedProjects()){
        //
        String projectLocation1=p1.getLocation().toOSString();
        // return file relative to project, since we may have the same file in different folders (Main.hs)
        if (fileName.startsWith( projectLocation1 )){
          fileName=fileName.substring( projectLocation1.length()+1 );
          break;
        }
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

  @Override
  public IRegisterGroup[] getRegisterGroups()  {
   return new IRegisterGroup[0];
  }

  @Override
  public IThread getThread() {
   return thread;
  }

  @Override
  public IVariable[] getVariables() throws DebugException {
    return getDebugTarget().getVariables(this);
  }

  @Override
  public boolean hasRegisterGroups() {
    return false;
  }

  @Override
  public boolean canStepInto() {
    return thread.canStepInto();
  }

  @Override
  public boolean canStepOver() {
    return thread.canStepOver();
  }

  @Override
  public boolean canStepReturn() {
    return thread.canStepReturn();
  }

  @Override
  public boolean isStepping() {
   return thread.isStepping();
  }

  @Override
  public void stepInto() throws DebugException {
    thread.stepInto();

  }

  @Override
  public void stepOver() {
   thread.stepOver();

  }

  @Override
  public void stepReturn()  {
    thread.stepReturn();

  }

  @Override
  public boolean canResume() {
    return thread.canResume();
  }

  @Override
  public boolean canSuspend() {
    return thread.canSuspend();
  }

  @Override
  public boolean isSuspended() {
    return thread.isSuspended();
  }

  @Override
  public void resume() throws DebugException {
    thread.resume();
  }

  @Override
  public void suspend()  {
    thread.suspend();
  }

  @Override
  public boolean canTerminate() {
   return thread.canTerminate();
  }

  @Override
  public boolean isTerminated() {
   return thread.isTerminated();
  }

  @Override
  public void terminate() throws DebugException {
    thread.terminate();

  }

}
