// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IImportVariable;

/** <p>implementation for <code>IImportVariable</code>.</p>
  *
  * @author Leif Frenzel
  */
class ImportVariable extends ImportSpecification implements IImportVariable {

  ImportVariable( final IImport imp ) {
    super( imp );
  }
}
