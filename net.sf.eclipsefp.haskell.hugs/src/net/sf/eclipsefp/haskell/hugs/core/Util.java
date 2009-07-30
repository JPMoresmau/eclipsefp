// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.core;

import java.io.File;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import net.sf.eclipsefp.haskell.hugs.HugsPlugin;
import net.sf.eclipsefp.haskell.hugs.core.preferences.IHugsPreferenceNames;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;

/** <p>contains common helping functionality.</p>
  *
  * @author Leif Frenzel
  */
public class Util {

  private static final String SEP = File.pathSeparator;

  public static String getCompilerExecutable() {
    String pref = Platform.getPreferencesService().getString( HugsPlugin.getPluginId(), IHugsPreferenceNames.EXECUTABLE_NAME, null, null );

    String result = "hugs";
    if( pref != null && !pref.equals( "" ) ) {
      result = pref;
    }
    return result;
  }

  public static String constructLibPath( final IHaskellProject hsProject ) {
    StringBuffer sbResult = new StringBuffer();
    IImportLibrary[] libs = hsProject.getImportLibraries();
    for( int i = 0; i < libs.length; i++ ) {
      if( i == 0 ) {
        sbResult.append( "-P" );
      } else {
        sbResult.append( SEP );
      }
      IPath path = libs[ i ].getPath();
      sbResult.append( path.toOSString() );
    }
    return sbResult.toString();
  }
}
