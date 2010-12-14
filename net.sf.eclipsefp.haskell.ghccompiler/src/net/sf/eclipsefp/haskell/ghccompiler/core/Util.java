// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import net.sf.eclipsefp.haskell.core.util.GHCSyntax;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/** <p>contains common helping functionality.</p>
  *
  * @author Leif Frenzel
  */
public class Util {
   /**
    * version number of GHC that deprecated -foption for -Xoption
    */
  public static String r6_8_1="6.8.1"; //$NON-NLS-1$

  public static String getCompilerExecutable() {
    IPath result = null;

    ICompilerManager msn = CompilerManager.getInstance();
    IHsImplementation impl = msn.getCurrentHsImplementation();
    if( impl != null && impl.getBinDir() != null ) {
      result = new Path( impl.getBinDir() );
      result = result.append( GHCSyntax.GHC );
    }
    return result == null ? GHCSyntax.GHC : result.toOSString();
  }

  public static String constructLibPath( final IFile... files ) {
    StringBuilder sbResult = new StringBuilder();
    /*IImportLibrary[] libs = hsProject.getImportLibraries();

    for( int i = 0; i < libs.length; i++ ) {
      if( i == 0 ) {
        sbResult.append( "-i" ); //$NON-NLS-1$
      } else {
        sbResult.append( File.pathSeparator );
      }
      IPath path = libs[ i ].getPath();
      sbResult.append( path.toOSString() );
    }*/
    // we only need hidden packages
    for (String s:ResourceUtil.getHiddenImportPackages( files )){
      sbResult.append(" -package "); //$NON-NLS-1$
      sbResult.append(s);
    }

    return sbResult.toString();
  }

  public static int compareTargets(final String target,final String version){
    String[] ts=target.split( "\\." ); //$NON-NLS-1$
    String[] vs=version.split( "\\." ); //$NON-NLS-1$
    int a=0;
    for (;a<vs.length;a++){
      if (a>=ts.length){
        return 1;
      }
      int it=Integer.parseInt( ts[a] );
      int iv=Integer.parseInt( vs[a] );
      if (iv>it){
        return -1;
      } else if (iv<it){
        return 1;
      }
    }
    if (a<ts.length){
      return 1;
    }

    return 0;
  }
}
