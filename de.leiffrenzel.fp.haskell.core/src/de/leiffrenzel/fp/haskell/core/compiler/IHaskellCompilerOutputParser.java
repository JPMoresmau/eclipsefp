// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.compiler;

/** <p>Implementors of IHaskellCompilerOutputParser can parse compiler output
  * and </p>
  * 
  * @author Leif Frenzel
  */
public interface IHaskellCompilerOutputParser {
  
  /** <p>chops the passed compiler output into items that can be displayed in
    * a table view.</p> */
  ICompilerOutputItem[] getItems( ICompilerOutput output );
}
