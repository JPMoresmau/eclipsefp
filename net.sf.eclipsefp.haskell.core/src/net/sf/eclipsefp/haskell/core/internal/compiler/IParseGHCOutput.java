// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.compiler;

import java.util.List;
import net.sf.eclipsefp.haskell.core.internal.project.provisionary.MarkerDesc;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;

/** <p>parses GHC output into marker descriptions.</p>
  *
  * @author Leif Frenzel
  */
public interface IParseGHCOutput {
  List<MarkerDesc> parse( String param ) throws CohatoeException;
}
