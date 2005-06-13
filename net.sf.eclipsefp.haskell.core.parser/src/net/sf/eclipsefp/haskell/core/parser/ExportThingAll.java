// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.IExportThingAll;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

/** <p>implementation for <code>IExportThingAll</code>.</p>
  *
  * @author Leif Frenzel
  */
class ExportThingAll extends ExportSpecification implements IExportThingAll {

  ExportThingAll( final IModule module ) {
    super( module );
  }
}
