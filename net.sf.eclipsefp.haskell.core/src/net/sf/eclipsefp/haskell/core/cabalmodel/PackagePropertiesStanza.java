// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

/** The Package Properties stanza for global, package properties. This corresponds
 * to the first stanza in the <code>.cabal</code> file.
  *
  * @author Leif Frenzel
  *
  */
public class PackagePropertiesStanza extends PackageDescriptionStanza {

  PackagePropertiesStanza(final int startLine) {
    super(null, null, startLine);
  }

  PackagePropertiesStanza(final int startLine, final int endLine ) {
    super(null, null, startLine, endLine );
  }

  @Override
  public String getName() {
    return getProperties().get( CabalSyntax.FIELD_NAME );
  }
}
