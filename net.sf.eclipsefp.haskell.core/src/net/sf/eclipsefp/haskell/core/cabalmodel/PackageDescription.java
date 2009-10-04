// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

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


  public List<PackageDescriptionStanza> getStanzas() {
    return stanzas;
  }

  public Map<String, List<PackageDescriptionStanza>> getStanzasBySourceDir(){
    Map<String, List<PackageDescriptionStanza>> ret=new HashMap<String, List<PackageDescriptionStanza>>();

    for (PackageDescriptionStanza pds:stanzas){
      String s=pds.getProperties().get( CabalSyntax.FIELD_HS_SOURCE_DIRS );
      if (s!=null && s.length()>0){
        for (String t:PackageDescriptionLoader.parseList( s )){
          if (t.length()>0){
            List<PackageDescriptionStanza> pdss=ret.get( t );
            if (pdss==null){
              pdss=new LinkedList<PackageDescriptionStanza>();
              ret.put( t, pdss );
            }
            pdss.add(pds);
          }
        }
      }
    }

    return ret;
  }
}
