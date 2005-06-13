// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.IExportModuleContent;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

/** <p>implementation for <code>IExportModuleContent</code>.</p>
  *
  * @author Leif Frenzel
  */
class ExportModuleContent extends ExportSpecification 
                          implements IExportModuleContent {

  ExportModuleContent( final IModule module ) {
    super( module );
  }
}
