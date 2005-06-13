// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IExportThingWithComponent</code>.</p>
  *
  * @author Leif Frenzel
  */
class ExportThingWithComponent implements IExportThingWithComponent {

  private final IExportThingWith expSpecification;
  private final String name;
  
  ExportThingWithComponent( final IExportThingWith expSpecification, 
                            final String name ) {
    this.expSpecification = expSpecification;
    this.name = name;
  }
  
  // interface methods of IExportThingWithComponent
  /////////////////////////////////////////////////

  public IExportThingWith getExportSpecification() {
    return expSpecification;
  }

  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public String getName() {
    return name;
  }

  public ICompilationUnit getCompilationUnit() {
    return expSpecification.getCompilationUnit();
  }

  // there is no source location for an export specification component
  public ISourceLocation getSourceLocation() {
    return null;
  }
  
  public IHaskellLanguageElement getParent() {
    return expSpecification;
  }
}
