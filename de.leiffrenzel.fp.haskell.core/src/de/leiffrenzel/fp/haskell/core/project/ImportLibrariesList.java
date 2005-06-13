// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.project;

import java.io.File;
import java.util.ArrayList;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.*;

import de.leiffrenzel.fp.haskell.core.HaskellCorePlugin;


/** <p>Encapsulates a list of import libraries that can be read from and 
  * saved to the core plugin preferences.</p>
  * 
  * @author Leif Frenzel
  */
public class ImportLibrariesList {
  
  private static final String PLUGIN_ID = HaskellCorePlugin.getPluginId(); 
  private static final String KEY = "PROJECT_IMPORT_LIBRARIES";
  
  private ArrayList importLibList;
  private IImportLibrary[] lastPersisted;

  private IProject project;


  /** constructs an import libraries list for use in the global workspace. */
  public ImportLibrariesList( final IProject project ) {
    this.project = project;
    importLibList = new ArrayList();
    load();
  }
  
  /** <p>saves the content of this ImportLibrariesList to the core 
    * preferences.</p> */
  public void save() throws CoreException {
    ProjectPropertiesEvent event = createProjectPropertiesEvent();
    
    project.setPersistentProperty( getQName(), encodeImportLibraries() );
    
    HaskellProjectManager.getInstance().broadcast( event );
    lastPersisted = getAll();
  }
  

  public IImportLibrary[] getAll() {
    IImportLibrary[] result = new IImportLibrary[ importLibList.size() ];
    importLibList.toArray( result );
    return result;
  }
  
  /** <p>adds the specified library to this list.</p> */
  public void add( final IImportLibrary library ) {
    importLibList.add( library );
  }

  /** <p>tests whether this list contains the specified library.</p> */
  public boolean contains( final IImportLibrary library ) {
    boolean result = false;
    for( int i = 0; !result && i < importLibList.size(); i++ ) {
      IImportLibrary lib = ( IImportLibrary )importLibList.get( i );
      result = lib.equals( library );
    }
    return result;
  }
  
  /** <p>removes the specified library from this list (if it was 
    * contained).</p> */
  public void remove( final IImportLibrary library ) {
    importLibList.remove( library );
  }
  
  public IImportLibrary createLibrary( final String path, 
                                       final boolean used ) {
    return new ImportLibrary( new Path( path ), used );
  }
  

  // helping methods
  //////////////////
  
  private void parseImportLibs( final String text ) {
    String markedText = text.replace( File.pathSeparatorChar, ';' );
    StringTokenizer stok = new StringTokenizer( markedText, ";" );
    while( stok.hasMoreTokens() ) {
      String token = stok.nextToken();
      ImportLibrary impLib = parse( token.trim() );
      if( impLib != null ) {
        importLibList.add( impLib );
      }
    }
  }
  
  private ImportLibrary parse( final String text ) {
    int commaIndex = text.lastIndexOf( ',' );
    boolean enabled = text.charAt( commaIndex + 1 ) == 't';
    String path = text.substring( 0, commaIndex );
    return new ImportLibrary( new Path( path ), enabled );
  }
  
  private String encodeImportLibraries() {
    StringBuffer buf = new StringBuffer();
    IImportLibrary[] libs = getAll();
    for( int i = 0; i < libs.length; i++ ) {
      if( i > 0 ) {
        buf.append( File.pathSeparatorChar );
      }
      buf.append( encodeImportLibrary( libs[ i ] ) );
    }
    return buf.toString();
  }

  private String encodeImportLibrary( final IImportLibrary lib ) {
    return   lib.getPath().toOSString() 
           + ","
           + ( lib.isUsed() ? "t" : "f" );
  }
  
  private QualifiedName getQName() {
    return new QualifiedName( PLUGIN_ID, KEY );
  }
  
  private void load(){
    String property = null;
    try {
      property = project.getPersistentProperty( getQName() );
    } catch( CoreException cex ) {
      String msg = "Could not read import libraries property for " + project;
      HaskellCorePlugin.log( msg, cex );
    }
    if( property != null && property.length() > 0 ) {
      parseImportLibs( property );
    }
    this.lastPersisted = getAll();
  }
  
  private ProjectPropertiesEvent createProjectPropertiesEvent() {
    IHaskellProject hsProject = HaskellProjectManager.get( project );
    String id = IHaskellProject.PROPERTY_IMPORT_LIBRARIES;
    ProjectPropertiesEvent event = new ProjectPropertiesEvent( hsProject, id );
    event.setOldValue( lastPersisted );
    event.setNewValue( getAll() );
    return event;
  }
}