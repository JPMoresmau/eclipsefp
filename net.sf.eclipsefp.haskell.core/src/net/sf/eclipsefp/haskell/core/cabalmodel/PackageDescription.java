// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
// Copyright (c) 2011 by Alejandro Serrano
package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.Component.ComponentType;

/** <p>The root of the package description model, represents the contents of a
  * <code>.cabal</code> file.</p>
  *
  * @author Leif Frenzel
  * @author Alejandro Serrano
  */
public class PackageDescription {

  private final List<PackageDescriptionStanza> stanzas;

  PackageDescription() {
    stanzas = new ArrayList<PackageDescriptionStanza>();
  }

  public PackageDescription(final String name){
    this();
    PackageDescriptionStanza pds = new PackagePropertiesStanza( 0 );
    stanzas.add( pds );
    pds.update( CabalSyntax.FIELD_NAME, name );
  }

  public List<PackageDescriptionStanza> getStanzas() {
    return stanzas;
  }

  public PackageDescriptionStanza getPackageStanza() {
    if (getStanzas().size()>0 && getStanzas().get(0) instanceof PackagePropertiesStanza){
      return getStanzas().get(0);
    }
    return null;
  }

  public PackageDescriptionStanza getLibraryStanza() {
    return getComponentStanza( new Component( ComponentType.LIBRARY, null, null, true ) );
  }

  public List<PackageDescriptionStanza> getExecutableStanzas() {
    Vector<PackageDescriptionStanza> result = new Vector<PackageDescriptionStanza>();
    for (PackageDescriptionStanza stanza : getStanzas()) {
      if (stanza.getType() == CabalSyntax.SECTION_EXECUTABLE) {
        result.add( stanza );
      }
    }
    return result;
  }

  public List<PackageDescriptionStanza> getTestSuiteStanzas() {
    Vector<PackageDescriptionStanza> result = new Vector<PackageDescriptionStanza>();
    for (PackageDescriptionStanza stanza : getStanzas()) {
      if (stanza.getType() == CabalSyntax.SECTION_TESTSUITE) {
        result.add( stanza );
      }
    }
    return result;
  }

  public PackageDescriptionStanza addStanza(final CabalSyntax type,final String name){
    int startLine=stanzas.get(stanzas.size()-1).getEndLine()+1;
    PackageDescriptionStanza pds=new PackageDescriptionStanza( type, name, startLine );
    pds.setEndLine( startLine+1 );
    stanzas.add( pds );
    return pds;
  }

  public void removeStanza(final PackageDescriptionStanza stanza) {
    boolean found = false;
    int diff = 0;

    for (PackageDescriptionStanza st : getStanzas()) {
      if (st == stanza) {
        diff = - (st.getEndLine() - st.getStartLine() + 1);
        found = true;
        continue;
      }
      if (found) {
        st.diffLine( diff );
      }
    }
    this.stanzas.remove( stanza );
  }

  public Map<String, List<PackageDescriptionStanza>> getStanzasBySourceDir(){
    Map<String, List<PackageDescriptionStanza>> ret=new HashMap<String, List<PackageDescriptionStanza>>();

    for (PackageDescriptionStanza pds:stanzas){
      Collection<String> sds=pds.getSourceDirs();
      for (String t:sds){
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

    return ret;
  }

  public Collection<String> getAllSourceDirs() {
    HashSet<String> result = new HashSet<String>();
    for (PackageDescriptionStanza pds:stanzas){
      Collection<String> sds=pds.getSourceDirs();
      result.addAll( sds );
    }
    return result;
  }

  public void dump(final Writer w) throws IOException {
    for (PackageDescriptionStanza pds:stanzas){
      pds.dump( w, 0 );
    }
  }

  public String dump(){
    StringWriter sw=new StringWriter();
    try {
      dump( sw );
      String s=sw.toString();
      return s;
    } catch (IOException ioe){
      // cannot happen
    }
    return null;
  }

  public PackageDescriptionStanza getComponentStanza(final Component c){
    for (PackageDescriptionStanza pds:stanzas){
      if (CabalSyntax.SECTION_LIBRARY.equals( pds.getType()) && c.getType().equals( ComponentType.LIBRARY )){
        return pds;
      }
      if (CabalSyntax.SECTION_EXECUTABLE.equals(pds.getType()) && c.getType().equals( ComponentType.EXECUTABLE ) && pds.getName().equals(c.getName())){
        return pds;
      }
      if (CabalSyntax.SECTION_TESTSUITE.equals(pds.getType()) && c.getType().equals( ComponentType.TESTSUITE ) && pds.getName().equals(c.getName())){
        return pds;
      }
    }
    return null;
  }

  public PackageDescriptionStanza getSameStanza(final PackageDescriptionStanza p){
    for (PackageDescriptionStanza pds:stanzas){
      if (pds.getType()==null && p.getType()==null){
        return pds;
      }
      if (CabalSyntax.SECTION_LIBRARY.equals( pds.getType()) && CabalSyntax.SECTION_LIBRARY.equals( p.getType())){
        return pds;
      }
      if (CabalSyntax.SECTION_EXECUTABLE.equals(pds.getType()) && CabalSyntax.SECTION_EXECUTABLE.equals(p.getType() ) && pds.getName().equals(p.getName())){
        return pds;
      }
      if (CabalSyntax.SECTION_TESTSUITE.equals(pds.getType()) && CabalSyntax.SECTION_TESTSUITE.equals(p.getType() ) && pds.getName().equals(p.getName())){
        return pds;
      }
    }
    return null;
  }
}
