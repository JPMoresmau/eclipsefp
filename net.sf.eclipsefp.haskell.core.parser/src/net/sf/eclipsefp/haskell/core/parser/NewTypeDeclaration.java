// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.INewTypeDeclaration;

/** <p>implementation for <code>INewTypeDeclaration</code>.</p>
  *
  * @author Leif Frenzel
  */
class NewTypeDeclaration extends Declaration implements INewTypeDeclaration {

  NewTypeDeclaration( final IHaskellLanguageElement parent, 
                      final IModule module ) {
    super( parent, module );
  }
}
