package net.sf.eclipsefp.haskell.core.cabalmodel;

/**
 * contributor to Cabal
 * @author jean-philippem
 *
 */
public interface ICabalContributor {

  /**
   * contribute to a package description on a new project
   * @param pd
   */
  void contributeOnNewProject(PackageDescription pd);
}
