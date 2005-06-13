// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>the implementation for <code>IImportSpecificatio</code>. This is the 
  * super class for the specific import specification classes.</p>
  *
  * @author Leif Frenzel
  */
abstract class ImportSpecification implements IImportSpecification {

  private String name;
  private final IImport imp;

  ImportSpecification( final IImport imp ) {
    this.imp = imp;
  }
  
  void setName( final String name ) {
    this.name = name;
  }
  
  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public String getName() {
    return name;
  }

  public ICompilationUnit getCompilationUnit() {
    return imp.getCompilationUnit();
  }
  
  public ISourceLocation getSourceLocation() {
    // we have no source location for import specifications
    return null;
  }
  
  public IHaskellLanguageElement getParent() {
    return imp;
  }
  
  // interface methods of IImportSpecification
  ////////////////////////////////////////////

  public IModule getModule() {
    return imp.getModule();
  }
  
  public IImport getImport() {
    return imp;
  }
}
