// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.compiler;

import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.provisionary.MarkerDesc;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;


public class TemporaryCompilerHelper {

  public static void applyOutput( final String content, final IFile file ) {
    // XXX in a later stage, we will want to collect contributions to an
    //     extension point here and run multiple output parsers on the content
    //     (e.g. Cabal and GHC and possibly others)

    CohatoeServer server = CohatoeServer.getInstance();
    IParseGHCOutput fun = server.createFunction( IParseGHCOutput.class );
    if( fun != null && content != null ) {
      List<MarkerDesc> descs = Collections.emptyList();
      try {
        descs = fun.parse( content );
      } catch( final CohatoeException cex ) {
        HaskellCorePlugin.log( cex );
      }
      for( MarkerDesc desc: descs ) {
        try {
          desc.applyToResource( file );
        } catch( final CoreException cex ) {
          HaskellCorePlugin.log( "Unable to apply marker to resource", cex ); //$NON-NLS-1$
        }
      }
    }
  }
}
