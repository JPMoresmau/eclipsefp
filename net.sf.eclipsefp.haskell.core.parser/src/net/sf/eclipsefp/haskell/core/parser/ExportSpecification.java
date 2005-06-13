// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>the implementation for <code>IExportSpecificatio</code>. This is the 
  * super class for the specific export specification classes.</p>
  *
  * @author Leif Frenzel
  */
abstract class ExportSpecification implements IExportSpecification {

  private final IModule module;
  private String name;

  ExportSpecification( final IModule module ) {
    this.module = module;
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
    return module.getCompilationUnit();
  }
  
  public ISourceLocation getSourceLocation() {
    // we have no source location for export specifications
    return null;
  }
  
  public IHaskellLanguageElement getParent() {
    return module;
  }
  
  // interface methods of IExportSpecification
  ////////////////////////////////////////////

  public IModule getModule() {
    return module;
  }
}
