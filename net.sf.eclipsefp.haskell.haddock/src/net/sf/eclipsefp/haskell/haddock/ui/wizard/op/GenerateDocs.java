// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard.op;

import java.io.File;
import java.io.IOException;
import java.util.List;

import net.sf.eclipsefp.haskell.haddock.HaddockPlugin;
import net.sf.eclipsefp.haskell.haddock.core.HaddockInfo;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.ui.IDebugUIConstants;

import de.leiffrenzel.fp.haskell.core.launch.CommandLineUtil;
import de.leiffrenzel.fp.haskell.core.launch.HaskellLaunchDelegate;
import de.leiffrenzel.fp.haskell.core.util.TracingUtil;

/** <p>the operation that performs the actual Haddock run.</p>
  *
  * @author Leif Frenzel
  */
public class GenerateDocs {

  private static final String PROCESS_TYPE_ID = GenerateDocs.class.getName();  
  private static boolean trace = HaddockPlugin.isTracing();
  
  private final HaddockInfo info;
  
  public GenerateDocs( final HaddockInfo info ) {
    this.info = info;
  }
  
  public boolean run() {
    boolean result = true;
    createDirs();
    try {
      String[] cmdLine = getCmdLine();
      Process process = Runtime.getRuntime().exec( cmdLine );
      if( process != null ) {
        try {
          ILaunchConfigurationWorkingCopy wc = createLaunch();
          wc.setAttribute( IDebugUIConstants.ATTR_PRIVATE, true );
          ILaunch newLaunch = new Launch( wc, ILaunchManager.RUN_MODE, null );
          String label = "Generating Haddock docs";
          IProcess proc = DebugPlugin.newProcess( newLaunch, process, label );
          proc.setAttribute( IProcess.ATTR_CMDLINE, 
                             CommandLineUtil.renderCommandLine( cmdLine) );
          proc.setAttribute( IProcess.ATTR_PROCESS_TYPE, PROCESS_TYPE_ID );
          getLauchManager().addLaunch( newLaunch );
        } catch( CoreException cex ) {
          HaddockPlugin.log( "Problem during docs generation", cex );
        }
      }
    } catch( IOException ioex ) {
      result = false;
      HaddockPlugin.log( "Problem during docs generation", ioex );
    }
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private void createDirs() {
    File outputDir = new File( info.getOutputDir() );
    outputDir.mkdirs();
  }

  private ILaunchManager getLauchManager() {
    return DebugPlugin.getDefault().getLaunchManager();
  }

  private ILaunchConfigurationWorkingCopy createLaunch() throws CoreException {
    String configTypeId = HaskellLaunchDelegate.class.getName();
    ILaunchConfigurationType configType 
      = getLauchManager().getLaunchConfigurationType( configTypeId );
    return configType.newInstance( null, "Haddock" );
  }

  private String[] getCmdLine() {
    List args = info.createCommand();
    if( trace ) {
      TracingUtil.dump( args );
    }
    return toArray( args );
  }

  private final String[] toArray( final List list ) {
    String[] result = new String[ list.size() ];
    list.toArray( result );
    return result;
  }
}
