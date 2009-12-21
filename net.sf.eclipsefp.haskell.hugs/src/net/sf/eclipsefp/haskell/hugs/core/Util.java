// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.core;

import java.io.File;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.hugs.HugsPlugin;
import net.sf.eclipsefp.haskell.hugs.core.preferences.IHugsPreferenceNames;
import org.eclipse.core.resources.IFile;
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

  public static String constructLibPath( final IFile... files ) {
    StringBuffer sbResult = new StringBuffer();

    for (String s:ResourceUtil.getImportPackages( files )){
      if( sbResult.length()==0 ) {
        sbResult.append( "-P" );
      } else {
        sbResult.append( SEP );
      }
      sbResult.append( s );

    }
    return sbResult.toString();
  }
}
