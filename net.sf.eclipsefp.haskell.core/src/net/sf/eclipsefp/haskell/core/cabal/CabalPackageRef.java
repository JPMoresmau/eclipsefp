package net.sf.eclipsefp.haskell.core.cabal;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


/**
 * a reference to a Cabal Package (different than CabalPackage)
 * @author JP Moresmau
 *
 */
public class CabalPackageRef {
  private String name;
  /**
   * all versions
   */
  private final ArrayList<String> versions=new ArrayList<>();
  /**
   * versions installed
   */
  private final Set<String> installed=new HashSet<>();

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
    List<CabalPackageVersion> ret=new ArrayList<>();
    for (int a=0;a<versions.size();a++){
      ret.add(new CabalPackageVersion( this, a,installed.contains( versions.get(a) )));
    }
    return ret;
  }

  @Override
  public String toString() {
    return name;
  }


  public Set<String> getInstalled() {
    return installed;
  }

  public boolean isInstalled(){
    return installed.size()>0; // we're installed if at least one version is installed
  }


}
