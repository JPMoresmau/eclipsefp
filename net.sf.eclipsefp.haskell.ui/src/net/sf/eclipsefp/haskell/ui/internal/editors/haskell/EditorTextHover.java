// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IFile;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class EditorTextHover implements IEditorTextHover {

  // interface methods of IEditorTextHover
  ////////////////////////////////////////

  public String computeInfoHover(
      final IFile srcRoot,
      final IFile file,
      final int line,
      final int column ) {
    String result = "[Got no answer from Haskell.]";
    String[] params = new String[] {
        srcRoot.getLocation().toOSString(),
        file.getLocation().toOSString(),
        String.valueOf( line ),
        String.valueOf( column )
      };
    try {
      CohatoeServer server = CohatoeServer.getInstance();
      String[] retVal = server.evaluate( IEditorTextHover.class, params );
      if( retVal != null && retVal.length == 1 ) {
        result = retVal[ 0 ];
      }
    } catch( CohatoeException cex ) {
      HaskellUIPlugin.log( cex );
    }
    return result;
  }
}
