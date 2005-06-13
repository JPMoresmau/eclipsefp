// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.ArrayList;
import java.util.List;

import de.leiffrenzel.fp.haskell.core.halamo.*;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ITypeSignature;

/** <p>implementation for <code>ITypeSignature</code>.</p>
  *
  * @author Leif Frenzel
  */
class TypeSignature extends Declaration implements ITypeSignature {

  // a type signature has no name itself (only a reference to one or more
  // identifiers to which the type is assigned), so we give a general dummy 
  // name here
  private static final String TYPESIG_NAME = "type signature";

  private final List identifiers = new ArrayList();
  
  TypeSignature( final IHaskellLanguageElement parent, final IModule module ) {
    super( parent, module );
  }
  
  void addIdentifier( final String identifier ) {
    identifiers.add( identifier );
  }

  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public String getName() {
    return TYPESIG_NAME;
  }
  
  
  // interface methods of ITyeSignature
  /////////////////////////////////////
  
  public String[] getIdentifiers() {
    return ( String[] )identifiers.toArray( new String[ identifiers.size() ] );
  }
}
