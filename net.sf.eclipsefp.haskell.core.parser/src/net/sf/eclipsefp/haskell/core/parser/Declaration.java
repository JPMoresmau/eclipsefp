// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>the implementation for <code>IDeclaration</code>. This is the super
  * class for the specific declaration classes.</p>
  *
  * @author Leif Frenzel
  */
abstract class Declaration implements IDeclaration {

  private final IModule module;
  private final IHaskellLanguageElement parent;
  private ISourceLocation sourceLocation;
  private String name;

  Declaration( final IHaskellLanguageElement parent, final IModule module ) {
    this.parent = parent;
    this.module = module;
  }
  
  void setSourceLocation( final ISourceLocation sourceLocation ) {
    this.sourceLocation = sourceLocation;
  }
  
  void setName( final String name ) {
    this.name = name;
  }
  
  
  // interface methods of IDeclaration
  ////////////////////////////////////
  
  public IModule getModule() {
    return module;
  }
  
  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public ICompilationUnit getCompilationUnit() {
    return module.getCompilationUnit();
  }
  
  public ISourceLocation getSourceLocation() {
    return sourceLocation;
  }
  
  public String getName() {
    return name;
  }
  
  public IHaskellLanguageElement getParent() {
    return parent;
  }
}
