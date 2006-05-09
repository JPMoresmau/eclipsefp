// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

/** <p>a component in an <code>IImportThingWith</code> element.</p>
  *
  * @author Leif Frenzel
  */
public interface IImportThingWithComponent extends IHaskellLanguageElement {

  /** <p>returns the <code>IImportThingWith</code> element to which this 
    * component belongs.</p> */
  IImportThingWith getImportSpecification();
}
