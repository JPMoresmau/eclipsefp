// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;
import de.leiffrenzel.fp.haskell.core.halamo.ITypeDeclaration;

/** <p>implementation for <code>ITypeDeclaration</code>.</p>
  *
  * @author Leif Frenzel
  */
class TypeDeclaration extends Declaration implements ITypeDeclaration {

  TypeDeclaration( final IHaskellLanguageElement parent,
                   final IModule module ) {
    super( parent, module );
  }
}
