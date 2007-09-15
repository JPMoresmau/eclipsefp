// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.core.model;

import java.util.ArrayList;
import java.util.List;

/** <p>The root of the package description model, represents the contents of a
  * <code>.cabal</code> file.</p>
  *
  * @author Leif Frenzel
  */
public class PackageDescription {

  private final List<PackageDescriptionStanza> stanzas;
  
  PackageDescription() {
    stanzas = new ArrayList<PackageDescriptionStanza>();
  }
  
  void addStanza( final PackageDescriptionStanza stanza ) {
    stanzas.add( stanza );
  }
  
  public PackageDescriptionStanza[] getStanzas() {
    return stanzas.toArray( new PackageDescriptionStanza[ stanzas.size() ] );
  }
}
