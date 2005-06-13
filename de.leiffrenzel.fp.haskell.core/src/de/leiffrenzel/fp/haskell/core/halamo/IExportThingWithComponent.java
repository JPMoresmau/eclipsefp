// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

/** <p>a component in an <code>IExportThingWith</code> element.</p>
  *
  * @author Leif Frenzel
  */
public interface IExportThingWithComponent extends IHaskellLanguageElement {

  /** <p>returns the <code>IExportThingWith</code> element to which this 
    * component belongs.</p> */
  IExportThingWith getExportSpecification();
}
