// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.configuratorpage;

import java.io.File;

import net.sf.eclipsefp.common.ui.configurator.IProbe;
import net.sf.eclipsefp.haskell.haddock.core.HaddockUtil;

import org.eclipse.core.runtime.IProgressMonitor;

/** <p>probes for a Haddock excutable on the system.</p>
  * 
  * @author Leif Frenzel
  */
public class HaddockExecutableProbe implements IProbe {
  
  // interface methods of IProbe
  //////////////////////////////
  
  public Object execute( final IProgressMonitor monitor ) {
    String result = null;
    monitor.beginTask( "Looking for a Haddock executable - ", 100 );
    
    monitor.subTask( "searching PATH" );
    result = query( "haddock" ); // with luck, it's on the path
    monitor.worked( 10 );
    
    if( result == null ) {
      monitor.subTask( "searching filesystem" );
      result = searchFileSystem( "haddock" );
      monitor.worked( 90 );
    }
    monitor.done();
    return result;
  }


  // helping methods
  //////////////////
  
  private String query( final String name ) {
    String result = null;
    String queried = HaddockUtil.queryHaddockExecutable( name );
    if(    queried.indexOf( "IOException" ) == -1
        && queried.indexOf( "Haddock" ) != -1 ) {
      result = name;
    }
    return result;
  }

  private String searchFileSystem( final String namePrefix ) {
    String result = null;
    File[] roots = File.listRoots();
    for( int i = 0; result == null && i < roots.length; i++ ) {
      File nextRoot = roots[ i ];
      if( isReadable( nextRoot ) ) { 
        result = searchRec( nextRoot, namePrefix );
      }
    } 
    return result;
  }
  
  private boolean isReadable( final File nextRoot ) {
    // on windows, ignore drives that are probably floppies
    return    !nextRoot.getPath().equalsIgnoreCase( "A:\\" )
           && !nextRoot.getPath().equalsIgnoreCase( "B:\\" )
           && nextRoot.canRead();
  }

  private String searchRec( final File dir, final String name ) {
    String result = null;
    File[] children = dir.listFiles();
    if( children != null ) {
      for( int i = 0; result == null && i < children.length; i++ ) {
        if( !children[ i ].isDirectory() ) {
          if( children[ i ].getName().startsWith( name ) ) {
            result = query( children[ i ].toString() );
          }
        } else {
          result = searchRec( children[ i ], name );
        }
      }
    }
    return result;
  }
}