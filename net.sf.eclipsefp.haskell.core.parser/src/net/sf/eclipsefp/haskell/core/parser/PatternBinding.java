// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IPatternBinding</code>.</p>
  *
  * @author Leif Frenzel
  */
class PatternBinding extends Declaration implements IPatternBinding {

  // a pattern binding has no name itself, so we give a general dummy name here
  // (possibly we could extend this to show a presentation of the pattern)
  private static final String PATTERN_BINDING_NAME = "pattern binding";

  PatternBinding( final IHaskellLanguageElement parent, 
                  final IModule module ) {
    super( parent, module );
  }
  
  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public String getName() {
    return PATTERN_BINDING_NAME;
  }
}
