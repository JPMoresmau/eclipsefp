// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IInstanceDeclaration</code>.</p>
  *
  * @author Leif Frenzel
  */
class InstanceDeclaration extends Declaration implements IInstanceDeclaration {

  InstanceDeclaration( final IHaskellLanguageElement parent, 
                       final IModule module ) {
    super( parent, module );
  }
}
