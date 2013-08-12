package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.uuagc.UuagcFile;
import net.sf.eclipsefp.haskell.core.uuagc.UuagcProjectManager;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;

/**
 * Runner for UU Attribute Grammar system.
 *
 * @author Alejandro Serrano
 */
public class UuagcRunner {

  private final IProject project;

  private static String fullPath;

  public static void setFullPath( final String path ) {
   fullPath = path;
 }

 public String getExecutableName() {
   if (fullPath!=null && fullPath.length()>0){
     return fullPath;
   }
   return  "uuagc"; //$NON-NLS-1$
 }

  public UuagcRunner( final IProject project ) {
    this.project = project;
  }

  public StringWriter selectStream( final StringWriter out,
      final StringWriter err ) {
    return out;
  }

  public String[] getExecutableAndArgs( final IResource resource ) {
    UuagcProjectManager mgr = new UuagcProjectManager( project );
    mgr.initFromProject();
    UuagcFile file = mgr.getElement( resource.getProjectRelativePath()
        .toPortableString() );
    if( file != null ) {
      ArrayList<String> r = new ArrayList<String>();
      r.add( "uuagc" ); //$NON-NLS-1$
      for( String option: file.getOptions() ) {
        if( option.indexOf( ' ' ) == -1 ) {
          r.add( "--" + option ); //$NON-NLS-1$
        } else {
          int spacePos = option.indexOf( ' ' );
          String initial = option.substring( 0, spacePos );
          String end = option.substring( spacePos + 1 );
          if( end.startsWith( "\"" ) ) { //$NON-NLS-1$
            // Remove quotes
            end = end.substring( 1, end.length() - 1 );
          }
          r.add( "--" + initial + "=" + end ); //$NON-NLS-1$//$NON-NLS-2$
        }
      }
      r.add( resource.getLocation().toOSString() );
      return r.toArray( new String[ r.size() ] );
    }
    // If no special information is found
    return new String[] { getExecutableName(), "--all", resource.getLocation().toOSString() }; //$NON-NLS-1$
  }

  public List<ProcessorError> run( final IResource resource ) {
    try {
      // Run the command
      StringWriter out = new StringWriter();
      //StringWriter err = new StringWriter();
      IPath path = resource.getLocation();
      new ProcessRunner().executeBlocking( path.toFile().getParentFile(), out,
          null, getExecutableAndArgs( resource ) );
      // Parse the output
      return OutputParser.errors( out.toString() );
    } catch( Throwable ex ) {
      return new ArrayList<ProcessorError>();
    }
  }
}
