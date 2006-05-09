// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

/** <p>An export specification for a module.</p>
  *
  * @author Leif Frenzel
  */
public interface IExportSpecification extends IHaskellLanguageElement {
  
  /** <p>returns the module to which this export specification belongs.</p> */
  IModule getModule();

}
