// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;
import net.sf.eclipsefp.haskell.ui.internal.views.outline.provisionary.TreeElement;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class HaskellOutline implements IHaskellOutline {

  // interface methods of IHaskellOutline
  ///////////////////////////////////////

  public List<ITreeElement> computeOutline( final String buffer ) {
    List<ITreeElement> result = Collections.emptyList();
    try {
      String[] params = new String[] { buffer };
      CohatoeServer server = CohatoeServer.getInstance();
      String[] retVal = server.evaluate( IHaskellOutline.class, params );
      result = TreeElement.unmarshal( retVal );
    } catch( final CohatoeException cex ) {
      HaskellUIPlugin.log( cex );
    }
    return result;
  }
}
