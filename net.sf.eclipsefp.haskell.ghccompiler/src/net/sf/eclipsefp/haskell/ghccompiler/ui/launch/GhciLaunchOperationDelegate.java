// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.launch;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import net.sf.eclipsefp.haskell.ghccompiler.core.CompilerParams;
import net.sf.eclipsefp.haskell.ghccompiler.core.Util;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import net.sf.eclipsefp.haskell.ui.launch.IInteractiveLaunchOperationDelegate;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.preference.IPreferenceStore;

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
    if( isUseGhcOptions() ) {
      cmdLine.addAll( compilerParams.construct() );
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

  private boolean isUseGhcOptions() {
    IPreferenceStore ps = GhcCompilerPlugin.getDefault().getPreferenceStore();
    return ps.getBoolean( IGhcPreferenceNames.GHCI_USES_GHC_OPTIONS );
  }

  private void addAll( final List<String> cmdLine, final IFile[] selectedFiles ) {
    for( int i = 0; i < selectedFiles.length; i++ ) {
      String path = selectedFiles[ i ].getLocation().toOSString();
      cmdLine.add( "\"" + path + "\"" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}
