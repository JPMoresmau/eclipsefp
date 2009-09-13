// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

/** <p>a stanza for a library.</p>
  *
  * @author Leif Frenzel
  * @deprecated
  */
public class LibraryStanza extends PackageDescriptionStanza {

  LibraryStanza( final String name, final int startLine, final int endLine ) {
    super(CabalSyntax.SECTION_LIBRARY, name, startLine, endLine );
  }
}
