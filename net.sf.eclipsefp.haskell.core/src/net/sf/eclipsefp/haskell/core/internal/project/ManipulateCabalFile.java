// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import org.eclipse.core.runtime.CoreException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

/** <p>general proxy for manipulating Cabal package descriptions (as
  * buffer content, not from the file system).</p>
  *
  * @author Leif Frenzel
  */
public class ManipulateCabalFile implements IManipulateCabalFile {

  // interface methods of IManipulateCabalFile
  ////////////////////////////////////////////

  public String get( final String buffer,
                     final Accessor acc ) throws CoreException {
    if( acc == null || buffer == null ) {
      throw new IllegalArgumentException();
    }
    return runFunction( new String[] { buffer, acc.toString() } );
  }

  public String set( final String buffer,
                     final Mutator mut,
                     final String newValue ) throws CoreException {
    if( mut == null || buffer == null || newValue == null ) {
      throw new IllegalArgumentException();
    }
    return runFunction( new String[] { buffer, mut.toString(), newValue } );
  }

  public String[] getAllSourceDirs( final String buffer ) throws CoreException {
    if( buffer == null ) {
      throw new IllegalArgumentException();
    }
    String[] result = new String[ 0 ];
    String[] params = new String[] {
        buffer, Accessor.GET_ALL_SOURCE_DIRS.toString()
    };
    CohatoeServer server = CohatoeServer.getInstance();
    String[] retVal = server.evaluate( IManipulateCabalFile.class, params );
    if( retVal != null ) {
      if( retVal[ 0 ].trim().length() == 0 ) {
        String msg = retVal.length > 1 ? retVal[ 1 ] : "No message"; //$NON-NLS-1$
        throw new InvalidCabalFileException( msg );
      }
      result = retVal;
    }
    return result;
  }


  // helping functions
  ////////////////////

  private String runFunction( final String[] params ) throws CoreException {
    String result = ""; //$NON-NLS-1$
    CohatoeServer server = CohatoeServer.getInstance();
    String[] retVal = server.evaluate( IManipulateCabalFile.class, params );
    if( retVal != null ) {
      if( retVal.length == 1 ) {
        result = retVal[ 0 ];
      } else if( retVal.length == 2 && retVal[ 0 ].trim().length() == 0 ) {
        // error! the second arg is the message
        throw new InvalidCabalFileException( retVal[ 1 ] );
      }
    }
    return result;
  }
}
