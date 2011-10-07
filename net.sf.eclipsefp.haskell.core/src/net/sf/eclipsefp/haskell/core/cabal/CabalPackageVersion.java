package net.sf.eclipsefp.haskell.core.cabal;

/**
 * A precise version of a cabal package
 * @author JP Moresmau
 *
 */
public class CabalPackageVersion {
  private final CabalPackageRef ref;
  private final int index;

  public CabalPackageVersion( final CabalPackageRef ref, final int index ) {
    super();
    this.ref = ref;
    this.index = index;
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
}
