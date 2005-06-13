// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.halamo;

import java.util.*;


/** <p>encapsulates the dependency graph for import dependencies between
  * compilation units in a Haskell project.</p>
  * 
  * @author Leif Frenzel
  */
class DependencyGraph {

  private Hashtable htDependencies = new Hashtable();
  
  void add( final ICompilationUnit cu ) {
    IModule[] modules = cu.getModules();
    for( int i = 0; i < modules.length; i++ ) {
      add( modules[ i ] );
    }
  }
  
  void remove( final ICompilationUnit cu ) {
    IModule[] modules = cu.getModules();
    for( int i = 0; i < modules.length; i++ ) {
      String name = modules[ i ].getName();
      htDependencies.remove( name );
    }
  }

  String[] getDependentModules( final ICompilationUnit cu ) {
    ArrayList list = new ArrayList();
    IModule[] modules = cu.getModules();
    for( int i = 0; i < modules.length; i++ ) {
      String name = modules[ i ].getName();
      Enumeration keys = htDependencies.keys();
      while( keys.hasMoreElements() ) {
        String key = ( String )keys.nextElement();
        HashSet element = ( HashSet )htDependencies.get( key );
        if( element.contains( name ) ) {
          list.add( key );
        }
      }
    }
    String[] result = new String[ list.size() ];
    list.toArray( result );
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private void add( final IModule module ) {
    String key = module.getName();
    HashSet dependentModules = new HashSet();
    IImport[] imports = module.getImports();
    for( int i = 0; i < imports.length; i++ ) {
      dependentModules.add( imports[ i ].getName() );
    }
    htDependencies.put( key, dependentModules );
  }
}