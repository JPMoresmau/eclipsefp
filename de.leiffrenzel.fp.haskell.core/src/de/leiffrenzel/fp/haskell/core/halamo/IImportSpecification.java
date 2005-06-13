// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/** <p>An import specification in an import declaration.</p>
  *
  * @author Leif Frenzel
  */
public interface IImportSpecification extends IHaskellLanguageElement {
  
  /** <p>returns the module to which this import specification belongs.</p> */
  IModule getModule();

  /** <p>returns the import declaration to which this import specification 
    * belongs.</p> */
  IImport getImport();
}
