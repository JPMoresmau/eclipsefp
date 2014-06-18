// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.DefaultStatus;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;


/** <p>validates the settings on the 'New Module' wizard page.</p>
  *
  * @author Leif Frenzel
  */
class Validator {

  /** Folders must be specified as qualified expression (with dots between
    * segments), not as paths (no slashes allowed). Segments must be valid
    * module qualifiers, that is, start with uppercase letter. Empty string
    * is ok. */
  static IStatus validateFolders( final String textFieldContent, final boolean qualify ) {
    DefaultStatus status = new DefaultStatus();
    if( textFieldContent.length() > 0 ) {
      if(    textFieldContent.indexOf( '/' ) != -1
          || textFieldContent.indexOf( '\\' ) != -1 ) {
        status.setError( UITexts.Validator_0 );
      } else if (qualify){
        String[] segments = getSegments( textFieldContent );
        boolean stillValid = true;
        for( int i = 0; stillValid && i < segments.length; i++ ) {
          stillValid = isValidModuleName( segments[ i ] );
        }
        if( !stillValid ) {
          status.setError( UITexts.Validator_1 );
        }
      }
    }
    return status;
  }

  static IStatus validateSourceFolder( final IContainer sourceContainer ) {
    DefaultStatus status = new DefaultStatus();
    if( sourceContainer == null ) {
     status.setError( UITexts.Validator_2 );
    }
    return status;
  }

  static IStatus validateModuleName( final ModuleCreationInfo info ) {
    String moduleName = info.getModuleName();
    DefaultStatus result = new DefaultStatus();
    if( moduleName.length() == 0 ) {
      // must not be empty
      result.setError( UITexts.Validator_3 );
    } else if( moduleName.indexOf( '.' ) != -1 ) {
      // must not be qualified
      result.setError( UITexts.Validator_4 );
    } else if( !isValidModuleName( moduleName ) ) {
      result.setError( UITexts.Validator_5 );
    } else if( existsAlready( info ) ) {
      // module must not yet exist
      result.setError( UITexts.Validator_6 );
    }
    return result;
  }


  // helping methods
  //////////////////

  private static boolean existsAlready( final ModuleCreationInfo info ) {
    return    existsAlreadyAs( info, FileUtil.EXTENSION_HS )
           || existsAlreadyAs( info, FileUtil.EXTENSION_LHS );
  }

  private static boolean existsAlreadyAs( final ModuleCreationInfo info,
                                          final String extension ) {
    String moduleName = info.getModuleName() + "." + extension; //$NON-NLS-1$
    IPath folders = info.getFolders();
    IPath fullPath = ( folders != null ) ? folders.append( moduleName )
                                         : new Path( moduleName );
    if (info.getSourceContainer()==null){
      return false;
    }
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
    List<String> list = new ArrayList<>();
    StringTokenizer tokenizer = new StringTokenizer( content, ".", false ); //$NON-NLS-1$
    while( tokenizer.hasMoreTokens() ) {
      list.add( tokenizer.nextToken() );
    }
    String[] result = new String[ list.size() ];
    list.toArray( result );
    return result;
  }
}