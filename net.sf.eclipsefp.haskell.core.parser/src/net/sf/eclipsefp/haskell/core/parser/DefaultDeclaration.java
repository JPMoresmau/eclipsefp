// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IDefaultDeclaration</code>.</p>
  *
  * @author Leif Frenzel
  */
class DefaultDeclaration extends Declaration implements IDefaultDeclaration {

  // a default declaration has no name itself (only a list of types), so we
  // give a general dummy name here
  private static final String DEFAULT_DECL_NAME = "default declaration";

  DefaultDeclaration( final IHaskellLanguageElement parent, 
                      final IModule module ) {
    super( parent, module );
  }
  
  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public String getName() {
    return DEFAULT_DECL_NAME;
  }
}
