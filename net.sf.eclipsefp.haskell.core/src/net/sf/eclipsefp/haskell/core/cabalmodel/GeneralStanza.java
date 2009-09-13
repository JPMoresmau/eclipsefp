// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

/** <p>a stanza for general information (the first stanza in the
  * <code>.cabal</code> file.</p>
  *
  * @author Leif Frenzel
  *
  */
public class GeneralStanza extends PackageDescriptionStanza {

  GeneralStanza(final int startLine, final int endLine ) {
    super(null, null, startLine, endLine );
  }

  @Override
  public String getName() {
    return getProperties().get( CabalSyntax.FIELD_NAME );
  }
}
