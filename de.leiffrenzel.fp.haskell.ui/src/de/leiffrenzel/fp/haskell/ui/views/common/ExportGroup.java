// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.views.common;

import de.leiffrenzel.fp.haskell.core.halamo.IModule;

/** <p>grouping object that is used on viewers to group an export 
  * specification in a module. It has it's own visual representation, exported
  * elements appear below it.</p>
  *
  * @author Leif Frenzel
  */
public class ExportGroup {

  private final IModule module;

  public ExportGroup( final IModule module ) {
    this.module = module;
  }
  
  public IModule getModule() {
    return module;
  }

}
