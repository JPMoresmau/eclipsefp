// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;


/** <p>A module declaration.</p>
  * 
  * @author Leif Frenzel
  */
public interface IModule extends IHaskellLanguageElement {

  /** <p>returns the export specifications for this <code>IModule</code>.</p> */
  IExportSpecification[] getExportSpecifications();
  /** <p>returns the import declarations for this <code>IModule</code>.</p> */
  IImport[] getImports();
  /** <p>returns the top-level declarations in this module.</p> */
  IDeclaration[] getDeclarations();
  /** <p>returns the compilation unit to which this module belongs.</p> */  
  ICompilationUnit getCompilationUnit();

}
