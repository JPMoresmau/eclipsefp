// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

/** <p>a stanza for general information (the first stanza in the
  * <code>.cabal</code> file.</p> 
  *
  * @author Leif Frenzel
  */
public class GeneralStanza extends PackageDescriptionStanza {

  GeneralStanza( final String name, final int startLine, final int endLine ) {
    super( name, startLine, endLine );
  }
}
