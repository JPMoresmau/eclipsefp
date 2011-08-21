package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.uuagc.UuagcFile;
import net.sf.eclipsefp.haskell.core.uuagc.UuagcProjectManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

public class UuagcRunner {

  private final IProject project;

  public UuagcRunner( final IProject project ) {
    this.project = project;
  }

  public InputStream selectStream( final Process p ) {
    return p.getInputStream();
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
    return new String[] {
        "uuagc", "--all", resource.getLocation().toOSString() }; //$NON-NLS-1$//$NON-NLS-2$
  }

  public List<ProcessorError> run( final IResource resource ) {
    try {
      // Run the command
      String[] cmdLine = getExecutableAndArgs( resource );
      Process p = Runtime.getRuntime().exec( cmdLine );
      // Parse the output
      p.waitFor();
      OutputParser parser = new OutputParser( selectStream( p ) );
      return parser.errors();
    } catch( Throwable ex ) {
      return new ArrayList<ProcessorError>();
    }
  }
}
