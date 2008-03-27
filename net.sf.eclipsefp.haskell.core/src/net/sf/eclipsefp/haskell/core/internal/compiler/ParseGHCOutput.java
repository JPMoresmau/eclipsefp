// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.compiler;

import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.provisionary.MarkerDesc;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

/** <p>parses GHC output into marker descriptions.</p>
  *
  * @author Leif Frenzel
  */
public class ParseGHCOutput implements IParseGHCOutput {

  // interface methods of IParseGHCOutput
  ///////////////////////////////////////

  public List<MarkerDesc> parse( final String content ) throws CohatoeException {
    List<MarkerDesc> result = Collections.emptyList();
    String[] params = new String[] { content };
    CohatoeServer server = CohatoeServer.getInstance();
    String[] retVal = server.evaluate( IParseGHCOutput.class, params );
    if( retVal != null ) {
      result = MarkerDesc.unmarshal( retVal );
      for( MarkerDesc markerDesc: result ) {
        markerDesc.setMarkerType( HaskellCorePlugin.ID_PROBLEM_MARKER );
      }
    }
    return result;
  }
}
