// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;


/** <p>An import statement.</p>
  * 
  * @author Leif Frenzel
  */
public interface IImport extends IHaskellLanguageElement {
  
  // TODO what about alias and qualified?
  
  /** <p>returns the module to which this import belongs.</p> */
  IModule getModule();
  /** <p>returns the name of the imported element.</p> */
  String getImportedElement();
  /** <p></p> */
  IImportSpecification[] getImportSpecifications();
  /** <p>given there is at least one import specification in this import
    * declaration, whether it is declared using the <code>hiding</code>
    * keyword.</p> */
  boolean isHiding();
}