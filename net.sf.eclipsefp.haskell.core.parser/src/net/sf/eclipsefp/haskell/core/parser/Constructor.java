// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IConstructor</code>.</p>
  *
  * @author Leif Frenzel
  */
class Constructor implements IConstructor {

  private final IModule module;
  private final IHaskellLanguageElement parent;
  private final String name;
  private ISourceLocation srcLoc;

  Constructor( final IModule module, 
               final IHaskellLanguageElement parent, 
               final String name ) {
    this.module = module;
    this.parent = parent;
    this.name = name;
  }

  void setSourceLocation( final ISourceLocation srcLoc ) {
    this.srcLoc = srcLoc;
  }

  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////

  public ICompilationUnit getCompilationUnit() {
    return module.getCompilationUnit();
  }

  public String getName() {
    return name;
  }

  public IHaskellLanguageElement getParent() {
    return parent;
  }

  public ISourceLocation getSourceLocation() {
    return srcLoc;
  }
}
