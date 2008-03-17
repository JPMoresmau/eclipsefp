// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.ui.launch;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.debug.ui.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.hugs.HugsPlugin;
import net.sf.eclipsefp.haskell.hugs.core.Util;

import org.eclipse.core.resources.IFile;

/** <p>implements a delegate for launching HUGS.</p>
  *
  * @author Leif Frenzel
  */
public class HugsLaunchOperationDelegate
       implements IInteractiveLaunchOperationDelegate {

  // interface methods of IInteractiveLaunchOperationDelegate
  ///////////////////////////////////////////////////////////

  public String[] createArguments( final IHaskellProject hsProject,
                                   final IFile[] selectedFiles ) {
    List<String> cmdLine = new ArrayList<String>();

    String libPath = Util.constructLibPath( hsProject );
    if( !libPath.equals( "" ) ) {
      cmdLine.add( libPath );
    }
    addAll( cmdLine, selectedFiles );
    String[] result = new String[ cmdLine.size() ];
    cmdLine.toArray( result );
    if( HugsPlugin.isTracing() ) {
      System.out.println( "Launching HUGS with arguments:" );
      HaskellCorePlugin.dump( cmdLine );
    }
    return result;
  }

  private void addAll( final List<String> cmdLine,
                       final IFile[] selectedFiles ) {
    for( int i = 0; i < selectedFiles.length; i++ ) {
      String path = selectedFiles[ i ].getLocation().toOSString();
      cmdLine.add( "\"" + path + "\"" );
    }
  }

  public String getExecutable() {
    return Util.getCompilerExecutable();
  }
}
