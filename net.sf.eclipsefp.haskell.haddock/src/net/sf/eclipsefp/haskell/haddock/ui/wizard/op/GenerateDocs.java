// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard.op;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.HaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.haddock.HaddockPlugin;
import net.sf.eclipsefp.haskell.haddock.core.HaddockInfo;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

/** <p>the operation that performs the actual Haddock run.</p>
  *
  * @author Leif Frenzel
  */
public class GenerateDocs {

 // private static final String PROCESS_TYPE_ID = GenerateDocs.class.getName();
  private static boolean trace = HaddockPlugin.isTracing();

  private final HaddockInfo info;

  public GenerateDocs( final HaddockInfo info ) {
    this.info = info;
  }

  public boolean run() {
    boolean result = true;
    createDirs();
 //   try {
      String[] cmdLine = getCmdLine();
      //Process process = Runtime.getRuntime().exec( cmdLine );
      //if( process != null ) {
        try {
          /*ILaunchConfigurationWorkingCopy wc = createLaunch();
          wc.setAttribute( IDebugUIConstants.ATTR_PRIVATE, true );
          wc.setAttribute( IDebugUIConstants.ATTR_CAPTURE_IN_CONSOLE, true );
          ILaunch newLaunch = new Launch( wc, ILaunchManager.RUN_MODE, null );
          String label = "Generating Haddock docs";
          IProcess proc = DebugPlugin.newProcess( newLaunch, process, label );
          proc.setAttribute( IProcess.ATTR_CMDLINE,
                             CommandLineUtil.renderCommandLine( cmdLine) );
          proc.setAttribute( IProcess.ATTR_PROCESS_TYPE, PROCESS_TYPE_ID );
          getLauchManager().addLaunch( newLaunch );*/
          String label = "Generating Haddock docs";
          IProject prj=null;
          if (info.getProjects().length==1){
            prj=info.getProjects()[0];
          }
          AbstractHaskellLaunchDelegate.runInConsole(prj, Arrays.asList(cmdLine), null, label,false );

        } catch( CoreException cex ) {
          HaddockPlugin.log( "Problem during docs generation", cex );
        }
     // }
//    } catch( IOException ioex ) {
//      result = false;
//      HaddockPlugin.log( "Problem during docs generation", ioex );
//    }
    return result;
  }


  // helping methods
  //////////////////

  private void createDirs() {
    File outputDir = new File( info.getOutputDir() );
    outputDir.mkdirs();
  }

//  private ILaunchManager getLauchManager() {
//    return DebugPlugin.getDefault().getLaunchManager();
//  }

//  private ILaunchConfigurationWorkingCopy createLaunch() throws CoreException {
    String configTypeId = HaskellLaunchDelegate.class.getName();
//    ILaunchConfigurationType configType
//      = getLauchManager().getLaunchConfigurationType( configTypeId );
//    return configType.newInstance( null, "Haddock" );
//  }

  private String[] getCmdLine() {
    List<String> args = info.createCommand();
    if( trace ) {
      HaskellCorePlugin.dump( args );
    }
    return args.toArray( new String[ args.size() ] );
  }
}
