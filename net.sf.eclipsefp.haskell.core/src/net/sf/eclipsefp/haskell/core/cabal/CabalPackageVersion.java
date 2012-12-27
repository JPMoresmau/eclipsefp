/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabal;

/**
 * A precise version of a cabal package
 * @author JP Moresmau
 *
 */
public class CabalPackageVersion implements Comparable<CabalPackageVersion> {
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


  public boolean isInstalled() {
    return installed;
  }
}
