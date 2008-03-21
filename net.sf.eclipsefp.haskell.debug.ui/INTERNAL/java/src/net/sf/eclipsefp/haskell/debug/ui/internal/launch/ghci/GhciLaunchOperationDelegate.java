// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.debug.ui.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import net.sf.eclipsefp.haskell.ghccompiler.core.CompilerParams;
import net.sf.eclipsefp.haskell.ghccompiler.core.Util;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Preferences;

/** <p>implements a delegate for launching GHCi.</p>
  *
  * @author Leif Frenzel
  */
public class GhciLaunchOperationDelegate
       implements IInteractiveLaunchOperationDelegate {

  private final CompilerParams compilerParams = new CompilerParams();

  // interface methods of IInteractiveLaunchOperationDelegate
  ///////////////////////////////////////////////////////////

  public String[] createArguments( final IHaskellProject hsProject,
                                   final IFile[] selectedFiles ) {
    List<String> cmdLine = new ArrayList<String>();

    cmdLine.add( "--interactive" ); //$NON-NLS-1$

    String libPath = Util.constructLibPath( hsProject );
    if( !"".equals( libPath ) ) { //$NON-NLS-1$
      cmdLine.add(libPath);
    }
    if( isPrefSet( IGhcPreferenceNames.GHCI_USES_GHC_OPTIONS ) ) {
      cmdLine.addAll( compilerParams.construct() );
    }
    if( isPrefSet( IGhcPreferenceNames.GHCI_SOURCE_FOLDERS ) ) {
      Set<IPath> sourcePaths = hsProject.getSourcePaths();
      for( IPath path: sourcePaths ) {
        cmdLine.add( "-i" + path.toString() ); //$NON-NLS-1$
      }
    }
    addAll( cmdLine, selectedFiles );
    String[] result = new String[ cmdLine.size() ];
    cmdLine.toArray( result );
    if( GhcCompilerPlugin.isTracing() ) {
      System.out.println( "Launching interactive session with arguments:" ); //$NON-NLS-1$
      HaskellCorePlugin.dump( cmdLine );
    }
    return result;
  }

  public String getExecutable() {
    return Util.getCompilerExecutable();
  }


  // helping methods
  //////////////////

  private boolean isPrefSet( final String name ) {
    Preferences prefs = GhcCompilerPlugin.getDefault().getPluginPreferences();
    return prefs.getBoolean( name );
  }

  private void addAll( final List<String> cmdLine, final IFile[] selectedFiles ) {
    for( int i = 0; i < selectedFiles.length; i++ ) {
      String path = selectedFiles[ i ].getLocation().toOSString();
      cmdLine.add( "\"" + path + "\"" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}
