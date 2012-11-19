/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.io.File;
import java.io.StringWriter;
import java.util.Map;
import java.util.Random;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.test.HTFParser;
import net.sf.eclipsefp.haskell.debug.core.test.ITestListener;
import net.sf.eclipsefp.haskell.debug.core.test.TestListenerManager;
import net.sf.eclipsefp.haskell.debug.core.test.TestResult;
import net.sf.eclipsefp.haskell.debug.core.test.TestSuite;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;


/**
 * Launch delegate for HTF tests
 * @author JP Moresmau
 *
 */
public class HTFLaunchDelegate extends ExecutableOrTestSuiteHaskellLaunchDelegate {

  private String filename = null;
  private TestResult root;
  private TestSuite suite;
  private ParseThread parseThread;

  private String getFilename() {
    if( filename == null ) {
      Random r = new Random( System.currentTimeMillis() );
      int n = r.nextInt();
      filename = HaskellDebugCore.getDefault().getStateLocation()
          .append( n + ".json" ).toOSString(); //$NON-NLS-1$
    }
    return filename;
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#createProcess(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, org.eclipse.core.runtime.IPath, java.lang.String[], java.io.File)
   */
  @Override
  protected IProcess createProcess( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IPath location, final String[] cmdLine,
      final File workingDir ) throws CoreException {
    /**
     * launch the same process, but with the list parameter, to get the list of tests without running them
     */

    String [] cmd2=new String[cmdLine.length+1];
    System.arraycopy( cmdLine, 0, cmd2, 0, cmd2.length-1 );
    cmd2[cmd2.length-1]="--list"; //$NON-NLS-1$
    StringWriter sw=new StringWriter();
    final String fname = getFilename();
    final File file = new File( fname );
    final IProject p=getProject( configuration );

    try {
      new ProcessRunner().executeBlocking( workingDir, sw, null, cmd2 );

      root=new TestResult( configuration.getName() );
      /** parse list **/
      HTFParser.parseTestList( root, file , p);

      suite=new TestSuite( root );

      for (ITestListener tl:TestListenerManager.getContributors().values()){
        tl.start(suite );
      }

    } catch (Exception ioe){
      throw new CoreException( new Status(IStatus.ERROR,HaskellDebugCore.getPluginId(),ioe.getLocalizedMessage(),ioe) );
    } finally {
      file.delete();
    }
    /**
     * create the parse thread
     */
    parseThread=new ParseThread( file, p );
    /**
     * super class will create the proper process
     */
    return super.createProcess( configuration, mode, launch, location, cmdLine,
        workingDir );
  }

  /**
   * delay between checks of the JSON output file
   */
  private static final long DELAY=1000;
  /**
   * thread regularly reading the HTF JSON output file
   * @author JP Moresmau
   *
   */
  private class ParseThread extends Thread{
    /**
     * continuation flag
     */
    private boolean goOn=true;
    /**
     * last modification date of the file we've checked
     */
    private long lastCheck=0;

    private final File file;
    private final IProject project;
    /**
     *
     */
    public ParseThread(final File f,final IProject p) {
     setDaemon( true );
     this.file=f;
     this.project=p;
    }

    /* (non-Javadoc)
     * @see java.lang.Thread#run()
     */
    @Override
    public void run() {
      while(goOn){
        try {
          long lm=file.lastModified();
          if (lm>lastCheck){
            lastCheck=lm;
            parseOutfile( file, project );
          }
        } catch (Exception e){
          HaskellDebugCore.log( e.getLocalizedMessage(), e );
        }
        try {
          Thread.sleep( DELAY );
        } catch (InterruptedException ie){
          //noop
        }
      }
    }
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#postProcessCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, org.eclipse.debug.core.model.IProcess)
   */
  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process ) {
    if (parseThread!=null){
      parseThread.start();
    }
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#preProcessCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, java.util.Map)
   */
  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final Map<String, String> processAttribs )
       {
    // NOOP

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#preProcessDefinitionCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch)
   */
  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode, final ILaunch launch )
      {
    // NOOP

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#postProcessFinished(org.eclipse.debug.core.ILaunchConfiguration)
   */
  @Override
  protected void postProcessFinished( final ILaunchConfiguration configuration )
      throws CoreException {
    /**
     * close the parse thread and checks if we need to reparse the file
     */
    long lastCheck=0;
    if (parseThread!=null){
      parseThread.goOn=false;
      parseThread.interrupt();
      lastCheck=parseThread.lastCheck;
      parseThread=null;
    }

    final String fname = getFilename();
    final File file = new File( fname );
    if (file.lastModified()>lastCheck){
      IProject p=getProject( configuration );
      try {
        parseOutfile( file, p );
      } catch (Exception ioe){
        throw new CoreException( new Status(IStatus.ERROR,HaskellDebugCore.getPluginId(),ioe.getLocalizedMessage(),ioe) );
      } finally {
        file.delete();
      }
    }

  }

  /**
   * parse the JSON output file
   * @param f
   * @param p
   * @throws Exception
   */
  private void parseOutfile(final File f,final IProject p) throws Exception{
    if (f.exists()){
      HTFParser.parseTestOutput( root, f , p);
      for (ITestListener tl:TestListenerManager.getContributors().values()){
        tl.start(suite );
      }
    }
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.ExecutableOrTestSuiteHaskellLaunchDelegate#getExtraArguments()
   */
  @Override
  protected String getExtraArguments() {
    return "--json --output-file=\""+getFilename()+"\" --colors=NO"; //$NON-NLS-1$ //$NON-NLS-2$
  }

}
