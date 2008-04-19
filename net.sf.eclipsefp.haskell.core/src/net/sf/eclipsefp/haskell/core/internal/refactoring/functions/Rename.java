// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.refactoring.functions;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class Rename implements IRename {

  public interface IReplaceEditDesc {
    IFile getFile();
    int getOffset();
    int getLength();
    String getReplacement();
  }

  public List<IReplaceEditDesc> performRename( final String content ) {
    List<IReplaceEditDesc> result = new ArrayList<IReplaceEditDesc>();
    try {
      String[] params = new String[] { content };
      CohatoeServer server = CohatoeServer.getInstance();
      String[] retVal = server.evaluate( IRename.class, params );
      unmarshal( retVal, result );
    } catch( final CohatoeException cohex ) {
      // we don't handle this here, just write it to the workspace log
      HaskellCorePlugin.getDefault().getLog().log( cohex.getStatus() );
    }
    return result;
  }


  // helping functions
  // //////////////////

  private void unmarshal( final String[] retVal,
      final List<IReplaceEditDesc> result ) {
    if( retVal != null ) {
      int index = 0;
      while( retVal.length > index + 3 ) {
        final IFile file = fromLocation( retVal[ index++ ] );
        final int offset = readInt( retVal[ index++ ] );
        final int length = readInt( retVal[ index++ ] );
        final String replacement = retVal[ index++ ];
        if( file != null && replacement != null ) {
          result.add( new IReplaceEditDesc() {
            public IFile getFile() {
              return file;
            }

            public int getOffset() {
              return offset;
            }

            public int getLength() {
              return length;
            }

            public String getReplacement() {
              return replacement;
            }
          } );
        }
      }
    }
  }

  private IFile fromLocation( final String location ) {
    IFile result = null;
    IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
    IFile[] files = wsRoot.findFilesForLocation( new Path( location ) );
    if( files != null ) {
      for( IFile file: files ) {
        if( file != null && file.exists() ) {
          result = file;
        }
      }
    }
    return result;
  }

  private int readInt( final String str ) {
    int result = 0;
    try {
      result = Integer.parseInt( str );
    } catch( final NumberFormatException numfex ) {
      // not a number...
    }
    return result;
  }
}
