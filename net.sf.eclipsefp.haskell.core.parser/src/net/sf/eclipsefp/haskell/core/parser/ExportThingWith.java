// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.ArrayList;
import java.util.List;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IExportThingWith</code>.</p>
  *
  * @author Leif Frenzel
  */
class ExportThingWith extends ExportSpecification implements IExportThingWith {

  private final List components;
  
  ExportThingWith( final IModule module ) {
    super( module );
    components = new ArrayList();
  }
  
  void addComponent( final IExportThingWithComponent component ) {
    components.add( component );
  }
  
  
  // interface methods of IExportThingWith
  ////////////////////////////////////////
  
  public IExportThingWithComponent[] getComponents() {
    int size = components.size();
    IExportThingWithComponent[] result = new IExportThingWithComponent[ size ];
    components.toArray( result );
    return result;
  }
}
