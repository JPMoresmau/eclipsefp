// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.ui.launch;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.preference.IPreferenceStore;

import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;
import de.leiffrenzel.fp.haskell.core.util.TracingUtil;
import de.leiffrenzel.fp.haskell.ghccompiler.GhcCompilerPlugin;
import de.leiffrenzel.fp.haskell.ghccompiler.core.CompilerParams;
import de.leiffrenzel.fp.haskell.ghccompiler.core.Util;
import de.leiffrenzel.fp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import de.leiffrenzel.fp.haskell.ui.launch.IInteractiveLaunchOperationDelegate;

/** <p>implements a delegate for launching GHCi.</p>
  * 
  * @author Leif Frenzel
  */
public class GhciLaunchOperationDelegate 
       implements IInteractiveLaunchOperationDelegate {
  
  private CompilerParams compilerParams = new CompilerParams();
  
  // interface methods of IInteractiveLaunchOperationDelegate
  ///////////////////////////////////////////////////////////
  
  public String[] createArguments( final IHaskellProject hsProject,
                                   final IFile[] selectedFiles ) {
    List cmdLine = new ArrayList();

    cmdLine.add( "--interactive" );

    String libPath = Util.constructLibPath( hsProject );
    if( !libPath.equals( "" ) ) {
      cmdLine.add(libPath);
    }
    if( isUseGhcOptions() ) {
      cmdLine.addAll( compilerParams.construct() );
    }
    addAll( cmdLine, selectedFiles );
    String[] result = new String[ cmdLine.size() ];
    cmdLine.toArray( result );
    if( GhcCompilerPlugin.isTracing() ) {
      System.out.println( "Launching interactive session with arguments:" );
      TracingUtil.dump( cmdLine );
    }
    return result;
  }
  
  public String getExecutable() {
    return Util.getCompilerExecutable();
  }


  // helping methods
  //////////////////
  
  private boolean isUseGhcOptions() {
    IPreferenceStore ps = GhcCompilerPlugin.getDefault().getPreferenceStore();
    return ps.getBoolean( IGhcPreferenceNames.GHCI_USES_GHC_OPTIONS );
  }
  
  private void addAll( final List cmdLine, final IFile[] selectedFiles ) {
    for( int i = 0; i < selectedFiles.length; i++ ) {
      String path = selectedFiles[ i ].getLocation().toOSString();
      cmdLine.add( "\"" + path + "\"" );
    }
  }
}
