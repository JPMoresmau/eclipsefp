// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

/** <p>An import specification for a class imported with some of its methods, 
  * or a datatype imported with some of its constructors.</p>
  *
  * @author Leif Frenzel
  */
public interface IImportThingWith extends IImportSpecification {

  /** <p>returns the components of the imported element that are explicitely 
    * imported.</p> */
  IImportThingWithComponent[] getComponents();
}
