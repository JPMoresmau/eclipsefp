// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;

/** <p>contains common helping functionality.</p>
  *
  * @author Leif Frenzel
  */
public class Util {
   /**
    * version number of GHC that deprecated -foption for -Xoption
    */
  public static String r6_8_1="6.8.1"; //$NON-NLS-1$


  /**
   * adds the command line options to include hidden packages
   */
  public static List<String> constructLibPath( final IFile... files ) {
    List<String> ret=new ArrayList<String>();

    // we only need hidden packages
    for (String s:ResourceUtil.getHiddenImportPackages( files )){
      ret.add("-package"); //$NON-NLS-1$
      ret.add(s);
    }

    return ret;
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
