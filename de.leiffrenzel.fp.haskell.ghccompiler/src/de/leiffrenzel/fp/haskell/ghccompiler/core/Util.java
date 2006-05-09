// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.core;

import net.sf.eclipsefp.common.core.util.QueryUtil;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.preference.IPreferenceStore;

import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import de.leiffrenzel.fp.haskell.ghccompiler.GhcCompilerPlugin;
import de.leiffrenzel.fp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;

/** <p>contains common helping functionality.</p>
  * 
  * @author Leif Frenzel
  */
public class Util implements IGhcParameters {

  public static String getCompilerExecutable() {
    IPreferenceStore ps = GhcCompilerPlugin.getDefault().getPreferenceStore();
    String pref = ps.getString( IGhcPreferenceNames.EXECUTABLE_NAME );
    
    String result = "ghc";
    if( pref != null && !pref.equals( "" ) ) {
      result = pref;
    }
    return result;
  }

  public static String queryGHCExecutable( final String name ) {
    StringBuffer sb = new StringBuffer();
    String executable = ( name == null ) ? "" : name; 
    sb.append( QueryUtil.query( executable, VERSION ) );
    sb.append( "\r\n" );
    sb.append( QueryUtil.query( executable, PRINT_LIBDIR ) );
    return sb.toString();
  }
  
  public static String constructLibPath( final IHaskellProject hsProject ) {
    StringBuffer sbResult = new StringBuffer();
    IImportLibrary[] libs = hsProject.getImportLibraries();
    for( int i = 0; i < libs.length; i++ ) {
      if( i == 0 ) {
        sbResult.append( "-i" );
      } else {
        sbResult.append( ":" );
      }
      IPath path = libs[ i ].getPath();
      sbResult.append( path.toOSString() );
    }
    return sbResult.toString();
  }
}
