// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.ArrayList;
import java.util.List;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IImportThingWith</code>.</p>
  *
  * @author Leif Frenzel
  */
class ImportThingWith extends ImportSpecification implements IImportThingWith {

  private final List components;
  
  ImportThingWith( final IImport imp ) {
    super( imp );
    components = new ArrayList();
  }
  
  void addComponent( final IImportThingWithComponent component ) {
    components.add( component );
  }
  
  
  // interface methods of IImportThingWith
  ////////////////////////////////////////
  
  public IImportThingWithComponent[] getComponents() {
    int size = components.size();
    IImportThingWithComponent[] result = new IImportThingWithComponent[ size ];
    components.toArray( result );
    return result;
  }
}
