// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;


/** <p>represents a top-level declaration in a module.</p>
  * 
  * @author Leif Frenzel
  */
public interface IDeclaration extends IHaskellLanguageElement {

  /** <p>returns the module to which this declaration belongs.</p> */
  IModule getModule();
  
}
