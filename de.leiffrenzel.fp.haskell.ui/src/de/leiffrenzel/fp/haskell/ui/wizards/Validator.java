// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.wizards;

import java.util.*;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.*;

import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;
import de.leiffrenzel.fp.haskell.ui.util.DefaultStatus;


/** <p>validates the settings on the 'New Module' wizard page.</p>
  * 
  * @author Leif Frenzel
  */
class Validator {

  /** Folders must be specified as qualified expression (with dots between 
    * segments), not as paths (no slashes allowed). Segments must be valid 
    * module qualifiers, that is, start with uppercase letter. Empty string 
    * is ok. */    
  static IStatus validateFolders( final String textFieldContent ) {
    DefaultStatus status = new DefaultStatus();
    if( textFieldContent.length() > 0 ) {
      if(    textFieldContent.indexOf( '/' ) != -1 
          || textFieldContent.indexOf( '\\' ) != -1 ) {
        status.setError( "Specify folders with '.', not as path." );
      } else {
        String[] segments = getSegments( textFieldContent );
        boolean stillValid = true;
        for( int i = 0; stillValid && i < segments.length; i++ ) {
          stillValid = isValidModuleName( segments[ i ] );
        }
        if( !stillValid ) {
          status.setError( "Invalid qualifier." );
        }
      }
    }
    return status;
  }
  
  static IStatus validateSourceFolder( final IContainer sourceContainer ) {
    DefaultStatus status = new DefaultStatus();
    if( sourceContainer == null ) {
     status.setError( "Select a source folder." );
    }
    return status;
  }
  
  static IStatus validateModuleName( final ModuleCreationInfo info ) {
    String moduleName = info.getModuleName();
    DefaultStatus result = new DefaultStatus();
    if( moduleName.length() == 0 ) {
      // must not be empty
      result.setError( "Module name is empty." );
    } else if( moduleName.indexOf( '.' ) != -1 ) {
      // must not be qualified
      result.setError( "Module name must not be qualified." );
    } else if( !isValidModuleName( moduleName ) ) {
      result.setError( "Invalid module name." );
    } else if( existsAlready( info ) ) {
      // module must not yet exist
      result.setError( "A module with this name exists already." );
    }
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private static boolean existsAlready( final ModuleCreationInfo info ) {
    return    existsAlreadyAs( info, ResourceUtil.EXTENSION_HS )
           || existsAlreadyAs( info, ResourceUtil.EXTENSION_LHS ); 
  }
  
  private static boolean existsAlreadyAs( final ModuleCreationInfo info,
                                          final String extension ) {
    String moduleName = info.getModuleName() + "." + extension;
    IPath folders = info.getFolders();
    IPath fullPath = ( folders != null ) ? folders.append( moduleName )
                                         : new Path( moduleName );
    IResource resource = info.getSourceContainer().findMember( fullPath );
    return resource != null && resource.exists(); 
  }
  
  /** Module names must start with an uppercase letter, apart from that,
    * we use just the Java conventions here, which will be fine in 
    * probably most of the cases. */
  private static boolean isValidModuleName( final String candidate ) {
    boolean result = true;
    result &= Character.isUpperCase( candidate.charAt( 0 ) );
    for( int i = 1; result && i < candidate.length(); i++ ) {
      char ch = candidate.charAt( i );
      result &= Character.isJavaIdentifierPart( ch );
    }
    return result;
  }
  
  private static String[] getSegments( final String content ) {
    List list = new ArrayList();
    StringTokenizer tokenizer = new StringTokenizer( content, ".", false );
    while( tokenizer.hasMoreTokens() ) {
      list.add( tokenizer.nextToken() );
    }
    String[] result = new String[ list.size() ];
    list.toArray( result );
    return result;
  }
}