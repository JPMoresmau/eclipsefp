// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

import java.util.HashSet;
import java.util.Hashtable;

import net.sf.eclipsefp.common.core.util.Assert;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

import de.leiffrenzel.fp.haskell.core.parser.IHaskellParser;
import de.leiffrenzel.fp.haskell.core.parser.ParserManager;
import de.leiffrenzel.fp.haskell.core.project.HaskellProjectManager;
import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;
import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;


/** <p>manages model elements for a project.</p>
  * 
  * @author Leif Frenzel
  */
class ProjectModel {

// TODO implement resource change listener and manage resource changes in 
// the project

  private IProject project;
  
  private Hashtable htCompilationUnits;
  private DependencyGraph dependencyGraph;

  ProjectModel( final IProject project ) {
    this.project = project;
    htCompilationUnits = new Hashtable();
    dependencyGraph = new DependencyGraph();
  }
  
  void initialize() throws CoreException {
    IContainer sources = ResourceUtil.getSourceFolder( project );
    initialize( sources );
  }
  
  ICompilationUnit getCompilationUnit( final IFile file ) {
    Assert.isTrue( ResourceUtil.hasHaskellExtension( file ) );
    
    Object obj = htCompilationUnits.get( getKey( file ) );
    ICompilationUnit result = ( ICompilationUnit )obj;
    if( result == null ) {
      result = createNewCompilationUnit( file );
    }
    return result;
  }

  public ICompilationUnit getByName( final String name ) {
    ICompilationUnit result = findParsed( name );
    if( result == null ) {
      result = findForFile( name );
    }
if( result == null ) {
  System.err.println(   "Could not find compilation unit for name '"
                      + name
                      + "' in project: "
                      + getHsProject().getResource().getName() );
}    
    return result;
  }
  
  /** returns all compilation units that depend (directly or indirectly,
    * on the passed compilation unit. */
  ICompilationUnit[] getDependentCUs( final ICompilationUnit cu ) {
    HashSet hsResult = new HashSet();
    getDependentCUsRec( cu, hsResult );
    ICompilationUnit[] result = new ICompilationUnit[ hsResult.size() ];
    hsResult.toArray( result );
    return result;
  }

  void invalidate( final IFile file ) {
    htCompilationUnits.remove( getKey( file ) );
  }
  
  
  // helping methods
  //////////////////

  private void getDependentCUsRec( final ICompilationUnit cu, 
                                   final HashSet hsResult ) {
    String[] names = dependencyGraph.getDependentModules( cu );
    for( int i = 0; i < names.length; i++ ) {
      ICompilationUnit unit = getByName( names[ i ] );
      if(    unit != null
          && !hsResult.contains( unit ) ) {
        hsResult.add( unit );
        getDependentCUsRec( unit, hsResult );
      }
    }
  }
  
  private ICompilationUnit findForFile( final String name ) {
    ICompilationUnit result = null;
    result = findForFile( name, ResourceUtil.EXTENSION_HS );
    if( result == null ) {
      result = findForFile( name, ResourceUtil.EXTENSION_LHS );
    }
    return result;
  }
  
  private ICompilationUnit findForFile( final String name,
                                        final String extension ) {
    ICompilationUnit result = null;
    IPath path = getPath( name, extension );
    IFile file = project.getFile( path );
    if( file != null && file.exists() ) {
      result = getCompilationUnit( file );
    }
    return result;
  }

  private ICompilationUnit findParsed( final String name ) {
    IPath hsPath = getPath( name, ResourceUtil.EXTENSION_HS );
    String key = hsPath.toString();
    ICompilationUnit result = ( ICompilationUnit )htCompilationUnits.get( key );
    
    if( result == null ) {
      IPath lhsPath = getPath( name, ResourceUtil.EXTENSION_LHS );
      result = ( ICompilationUnit )htCompilationUnits.get( lhsPath.toString() );
    }
    return result;
  }

  private IPath getPath( final String name,
                         final String extension ) {
    String fileName = name.replace( '.', '/' ) + "." + extension;
    return getHsProject().getSourcePath().append( fileName );
  }
  
  private ICompilationUnit createNewCompilationUnit( final IFile file ) {
    IHaskellParser parser = ParserManager.getInstance().getParser();
    ICompilationUnit result = null;
    try {
      if( parser.canParse() ) {
        result = parser.parse( file );
      }
    } catch( CoreException ex ) {
      // TODO where to handle the exception case?
      ex.printStackTrace();
    }
    if( result != null ) {
      htCompilationUnits.put( getKey( file ), result );
      dependencyGraph.add( result );
    }
    return result;
  }
  
  /** creates a project-wide unique key for the compilation unit under which 
    * it is known in this project model. */
  private String getKey( final IFile file ) {
    return file.getProjectRelativePath().toString();    
  }
  
  private IHaskellProject getHsProject() {
    return HaskellProjectManager.get( this.project ); 
  }
  
  private void initialize( final IContainer container ) throws CoreException {
    IResource[] members = container.members();
    for( int i = 0; i < members.length; i++ ) {
      IResource resource = members[ i ];
      if( resource.getType() == IResource.FILE ) {
        IFile file = ( IFile )resource;
        if( ResourceUtil.hasHaskellExtension( file ) ) {
          createNewCompilationUnit( file );
        }
      } else if( resource.getType() == IResource.FOLDER ) {
        initialize( ( IFolder )resource );
      }
    }
  }
}