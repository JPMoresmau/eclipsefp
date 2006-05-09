// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

/** <p>An export specification for a class exported with some of its methods, 
  * or a datatype exported with some of its constructors.</p>
  *
  * @author Leif Frenzel
  */
public interface IExportThingWith extends IExportSpecification {

  /** <p>returns the components of the exported element that are explicitely 
    * exported.</p> */
  IExportThingWithComponent[] getComponents();

}
