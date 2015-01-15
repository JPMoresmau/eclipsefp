/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabal;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;

/**
 * A precise version of a cabal package
 * @author JP Moresmau
 *
 */
public class CabalPackageVersion implements Comparable<CabalPackageVersion> {

  public static enum Restriction {
    NONE,
    MAJOR,
    MAJOR_FROM_MINOR,
    MINOR
  };

  private final CabalPackageRef ref;
  private final int index;
  /**
   * are we installed?
   */
  private final boolean installed;

  public CabalPackageVersion( final CabalPackageRef ref, final int index , final boolean installed) {
    super();
    this.ref = ref;
    this.index = index;
    this.installed=installed;
  }


  public CabalPackageRef getRef() {
    return ref;
  }

  @Override
  public String toString(){
    return ref.getVersions().get(index);
  }

  public boolean isLast(){
    return index==ref.getVersions().size()-1;
  }

  @Override
  public int compareTo( final CabalPackageVersion o ) {
    String s1=toString();
    String s2=o.toString();
    return compare(s1,s2);
  }

  public static int compare( final String s1,final String s2 ) {
    String[] ss1=s1.split( "\\." ); //$NON-NLS-1$
    String[] ss2=s2.split( "\\." ); //$NON-NLS-1$
    int a=0;
    for (;a<ss1.length;a++){
      if (a>=ss2.length){
        return 1;
      }
      int i1=Integer.parseInt( ss1[a] );
      int i2=Integer.parseInt( ss2[a] );
      if (i1<i2){
        return -1;
      } else if (i2<i1){
        return 1;
      }
    }
    if (a<ss2.length){
      return -1;
    }
    return 0;
  }

  /**
   * get the range including all the versions that have the same major components
   * @param s1 the precise version we want to get the full range for
   * @return the range ins Cabal syntax
   */
  public static String getMajorRange(final String s1){
    String[] ss1=s1.split( "\\." ); //$NON-NLS-1$
    if (ss1.length>1){
      try {
        return ">="+ ss1[0]+"."+ss1[1]+" && <"+ss1[0]+"."+(Integer.parseInt( ss1[1])+1);
      } catch (NumberFormatException nfe){
        HaskellCorePlugin.log( nfe );
      }
    }
    return "";
  }

  /**
   * get the range including all the versions that have the same minor components
   * @param s1 the precise version we want to get the full range for
   * @return the range ins Cabal syntax
   */
  public static String getMinorRange(final String s1){
    String[] ss1=s1.split( "\\." ); //$NON-NLS-1$
    if (ss1.length>2){
      try {
        return ">="+ ss1[0]+"."+ss1[1]+"."+ss1[2]+" && <"+ss1[0]+"."+ss1[1]+"."+(Integer.parseInt( ss1[2])+1);
      } catch (NumberFormatException nfe){
        HaskellCorePlugin.log( nfe );
      }
    } else if(ss1.length>1){
      try {
        return ">="+ ss1[0]+"."+ss1[1]+" && <"+ss1[0]+"."+ss1[1]+".1";
      } catch (NumberFormatException nfe){
        HaskellCorePlugin.log( nfe );
      }
    }
    return "";
  }

  /**
   * get the range including all the versions that have the same major components but start from the provided minor versin
   * @param s1 the precise version we want to get the full range for
   * @return the range ins Cabal syntax
   */
  public static String getMajorRangeFromMinor(final String s1){
    String[] ss1=s1.split( "\\." ); //$NON-NLS-1$
    if (ss1.length>2){
      try {
        return ">="+ ss1[0]+"."+ss1[1]+"."+ss1[2]+" && <"+ss1[0]+"."+(Integer.parseInt( ss1[1])+1);
      } catch (NumberFormatException nfe){
        HaskellCorePlugin.log( nfe );
      }
    } else {
      return getMajorRange( s1 );
    }
    return "";
  }

  /**
   * get the package name + range version
   * @param name
   * @param version
   * @param r
   * @return
   */
  public static String getRange (final String name,final String version,final Restriction r){
    switch( r ) {
      case MAJOR:
        return name+" "+getMajorRange( version );
      case MAJOR_FROM_MINOR:
        return name+" "+getMajorRangeFromMinor( version );
      case MINOR:
        return name+" "+getMinorRange( version );
      default:
        return name;
    }
  }

  public boolean isInstalled() {
    return installed;
  }
}
