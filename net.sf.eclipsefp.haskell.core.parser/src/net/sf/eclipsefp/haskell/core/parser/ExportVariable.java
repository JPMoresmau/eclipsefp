// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.IExportVariable;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

/** <p>implementation for <code>IExportVariable</code>.</p>
  *
  * @author Leif Frenzel
  */
class ExportVariable extends ExportSpecification implements IExportVariable {

  ExportVariable( final IModule module ) {
    super( module );
  }
}
