// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.views.common;

import net.sf.eclipsefp.haskell.core.halamo.IModule;

/** <p>grouping object that is used on viewers to group an export
  * specification in a module. It has its own visual representation, exported
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
