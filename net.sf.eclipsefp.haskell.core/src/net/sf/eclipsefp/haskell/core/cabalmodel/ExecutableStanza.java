// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

/** <p>a stanza for an executable.</p>
  *
  * @author Leif Frenzel
  * @deprecated
  */
public class ExecutableStanza extends PackageDescriptionStanza {

  ExecutableStanza( final String name,
                    final int startLine,
                    final int endLine ) {
    super(CabalSyntax.SECTION_EXECUTABLE, name, startLine, endLine );
  }
}
