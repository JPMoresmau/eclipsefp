// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IImportThingWithComponent</code>.</p>
  *
  * @author Leif Frenzel
  */
class ImportThingWithComponent implements IImportThingWithComponent {

  private final IImportThingWith impSpecification;
  private final String name;
  
  ImportThingWithComponent( final IImportThingWith impSpecification, 
                            final String name ) {
    this.impSpecification = impSpecification;
    this.name = name;
  }
  
  // interface methods of IImportThingWithComponent
  /////////////////////////////////////////////////

  public IImportThingWith getImportSpecification() {
    return impSpecification;
  }

  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public String getName() {
    return name;
  }

  public ICompilationUnit getCompilationUnit() {
    return impSpecification.getCompilationUnit();
  }

  // there is no source location for an import specification component
  public ISourceLocation getSourceLocation() {
    return null;
  }
  
  public IHaskellLanguageElement getParent() {
    return impSpecification;
  }
}
