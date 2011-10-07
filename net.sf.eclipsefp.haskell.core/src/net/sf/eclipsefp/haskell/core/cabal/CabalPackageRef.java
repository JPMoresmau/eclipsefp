package net.sf.eclipsefp.haskell.core.cabal;

import java.util.ArrayList;
import java.util.List;


/**
 * a reference to a Cabal Package (different than CabalPackage)
 * @author JP Moresmau
 *
 */
public class CabalPackageRef {
  private String name;
  private final ArrayList<String> versions=new ArrayList<String>();

  public String getName() {
    return name;
  }

  public void setName( final String name ) {
    this.name = name;
  }

  public ArrayList<String> getVersions() {
    return versions;
  }

  public List<CabalPackageVersion> getCabalPackageVersions(){
    List<CabalPackageVersion> ret=new ArrayList<CabalPackageVersion>();
    for (int a=0;a<versions.size();a++){
      ret.add(new CabalPackageVersion( this, a));
    }
    return ret;
  }

  @Override
  public String toString() {
    return name;
  }


}
