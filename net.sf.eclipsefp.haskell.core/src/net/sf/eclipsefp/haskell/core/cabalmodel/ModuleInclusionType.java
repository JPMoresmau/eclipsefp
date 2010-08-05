package net.sf.eclipsefp.haskell.core.cabalmodel;

/**
 * Ways a module can be included in a cabal section: included, exposed by a library, main for an executable, or missing (not referenced)
 * @author JP Moresmau
 *
 */
public enum ModuleInclusionType {
  /**
   * module is included
   */
  INCLUDED,
  /**
   * module is exposed by a library
   */
  EXPOSED,
  /**
   * EXE
   */
  MAIN,
  /**
   * module is not present
   */
  MISSING
}
