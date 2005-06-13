// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.IExportAbsolute;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

/** <p>implementation for <code>IExportAbsolute</code>.</p>
  *
  * @author Leif Frenzel
  */
class ExportAbsolute extends ExportSpecification implements IExportAbsolute {

  ExportAbsolute( final IModule module ) {
    super( module );
  }
}
