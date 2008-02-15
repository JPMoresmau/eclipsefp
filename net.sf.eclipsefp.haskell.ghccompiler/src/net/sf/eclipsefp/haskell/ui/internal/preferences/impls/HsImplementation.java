// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.impls;

import java.io.File;
import java.io.IOException;
import net.sf.eclipsefp.haskell.core.util.QueryUtil;
import net.sf.eclipsefp.haskell.ghccompiler.core.IGhcParameters;
import net.sf.eclipsefp.haskell.ui.util.DefaultStatus;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.util.NLS;


class HsImplementation implements IHsImplementation {

  private String name;
  private HsImplementationType type;
  private String version;
  private String libDir;
  private String binDir;

  private IStatus[] statuss;

  void setVersion( final String version ) {
    this.version = version;
  }

  void setType( final HsImplementationType type ) {
    invalidate();
    this.type = type;
  }

  void setName( final String name ) {
    invalidate();
    this.name = name;
  }

  void setBinDir( final String binDir ) {
    invalidate();
    this.binDir = binDir;
  }

  IStatus[] validate() {
    if( statuss == null ) {
      internalValidate();
    }
    return statuss;
  }


  // interface methods of IInstalledHaskellImplementation
  // /////////////////////////////////////////////////////

  public String getName() {
    return name;
  }

  public HsImplementationType getType() {
    return type;
  }

  public String getVersion() {
    return version;
  }

  public String getLibDir() {
   return libDir;
  }

  public String getBinDir() {
    return binDir;
  }


  // helping functions
  ////////////////////

  private void invalidate() {
    this.statuss = null;
    libDir = null;
    version = null;
  }

  private IStatus validateName() {
    DefaultStatus result = new DefaultStatus();
    if( name == null || name.trim().length() == 0 ) {
      result.setError( "Please enter a name for this installation" );
    } else {
      IWorkspace workspace = ResourcesPlugin.getWorkspace();
      IStatus tempStatus = workspace.validateName( name, IResource.FILE );
      if( !tempStatus.isOK() ) {
        String msg = "Name must be a valid file name. {0}";
        String binding = tempStatus.getMessage();
        result.setError( NLS.bind( msg, new String[] { binding } ) );
      }
    }
    return result;
  }

  private IStatus validateLocation() {
    DefaultStatus result = new DefaultStatus();
    File file = null;
    if( binDir == null || binDir.length() == 0 ) {
      result.setError( "Please enter a location" );
    } else {
      file = new File( binDir );
      if( !file.exists() || !file.isDirectory() ) {
        result.setError( "Location does not exist or is not a directory" );
      }
    }
    return result;
  }

  private IStatus validateExecutable() {
    DefaultStatus result = new DefaultStatus();
    IPath path = new Path( binDir );
    path = path.append( "ghc" );
    if( Platform.getOS().equals( Platform.OS_WIN32 ) ) {
      path.addFileExtension( "exe" );
    }
    try {
      String param = IGhcParameters.NUMERIC_VERSION;
      String query = QueryUtil.queryEx( path.toOSString(), param );
      if( query != null && query.trim().length() > 0 ) {
        this.version = query.trim();
      }
    } catch( IOException ex ) {
      result.setError( ex.getMessage() );
    }

    if( result.isOK() ) {
      try {
        String param = IGhcParameters.PRINT_LIBDIR;
        String query = QueryUtil.queryEx( path.toOSString(), param );
        if( query != null && query.trim().length() > 0 ) {
          this.libDir = query.trim();
        }
      } catch( IOException ex ) {
        result.setError( ex.getMessage() );
      }

    }
    return result;

  }

  private void internalValidate() {
    statuss = new IStatus[] {
      validateName(), validateLocation(), new DefaultStatus()
    };
    if(    !statuss[ 0 ].matches( IStatus.ERROR )
        && !statuss[ 1 ].matches(  IStatus.ERROR ) ) {
      statuss[ 2 ] = validateExecutable(); // time-consuming, do this only as needed
    }
  }
}
