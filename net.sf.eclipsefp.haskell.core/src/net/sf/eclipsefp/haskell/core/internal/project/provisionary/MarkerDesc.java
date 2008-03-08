// Copyright (c) 2006-2008 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.internal.project.provisionary;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;


/** <p>a lightweight marker descriptor, with the most common problem marker
  * attributes. This class can be used for conveniently transferring
  * marker information to and from Haskell code.</p>
  *
  * @author Leif Frenzel
  */
public class MarkerDesc implements ICohatoeData {

  public static final int UNSPECIFIED = -1;

  private static enum Severity {
    Error, Warning, Info
  }

  private String fileName;
  private int line = UNSPECIFIED;
  private String message = ""; //$NON-NLS-1$
  private int charStart = UNSPECIFIED;
  private int charEnd = UNSPECIFIED;
  private int severity = IMarker.SEVERITY_INFO;
  private String markerID;

  public static List<MarkerDesc> unmarshal( final String[] args ) {
    List<MarkerDesc> result = new ArrayList<MarkerDesc>();
    // policy: we make the best out of what we receive, if a sequence is not
    // complete, we fill in default values
    if( args != null ) {
      int index = 0;
      while( args.length > index ) {
        MarkerDesc md = readMarkerDesc( index, args );
        if( md != null ) {
          result.add( md );
        }
        index += 6;
      }
    }
    return result;
  }

  public void setMarkerType( final String id ) {
    if( id == null || id.trim().length() == 0 ) {
      throw new IllegalArgumentException();
    }
    this.markerID = id;
  }

  public void applyToResource() throws CoreException {
    if( markerID == null ) {
      throw new IllegalStateException( "No marker ID set" ); //$NON-NLS-1$
    }
    List<IResource> ress = findResource( Path.fromOSString( fileName ) );
    for( IResource resource: ress ) {
      if( resource != null && resource.isAccessible() ) {
        IMarker marker = resource.createMarker( markerID );
        marker.setAttribute( IMarker.MESSAGE, message );
        marker.setAttribute( IMarker.LINE_NUMBER, line );
        marker.setAttribute( IMarker.CHAR_START, charStart );
        marker.setAttribute( IMarker.CHAR_END, charEnd );
        marker.setAttribute( IMarker.SEVERITY, severity );
      }
    }
  }




  // attribute getters and setters
  ////////////////////////////////

  public String getFileName() {
    return fileName;
  }

  public int getLine() {
    return line;
  }

  public String getMessage() {
    return message;
  }

  public int getCharStart() {
    return charStart;
  }

  public int getCharEnd() {
    return charEnd;
  }

  public int getSeverity() {
    return severity;
  }

  // interface methods of ICohatoeData
  ////////////////////////////////////

  public List<String> marshal() {
    List<String> result = new ArrayList<String>();
    result.add( fileName );
    result.add( String.valueOf( line ) );
    result.add( message );
    result.add( String.valueOf( charStart ) );
    result.add( String.valueOf( charEnd ) );
    switch( severity ) {
      case IMarker.SEVERITY_INFO:
        result.add( Severity.Info.toString() );
        break;
      case IMarker.SEVERITY_WARNING:
        result.add( Severity.Warning.toString() );
        break;
      case IMarker.SEVERITY_ERROR:
        result.add( Severity.Error.toString() );
        break;
    }
    return result;
  }


  // helping functions
  ////////////////////

  private static MarkerDesc readMarkerDesc( final int start,
                                            final String[] args ) {
    MarkerDesc result = null;
    if( args[ start ] != null && args[ start ].trim().length() > 0 ) {
      int index = start;
      result = new MarkerDesc();
      result.fileName = args[ index++ ];

      if( args.length > index ) {
        result.line = readInt( args[ index++ ] );
      }
      if( args.length > index ) {
        result.message = args[ index++ ];
      }
      if( args.length > index ) {
        result.charStart = readInt( args[ index++ ] );
      }
      if( args.length > index ) {
        result.charEnd = readInt( args[ index++ ] );
      }
      if( args.length > index ) {
        result.severity = readSeverity( args[ index++ ] );
      }
    }
    return result;
  }

  private static int readSeverity( final String content ) {
    int result = IMarker.SEVERITY_INFO;
    try {
      switch( Severity.valueOf( content ) ) {
        case Error:
          result = IMarker.SEVERITY_ERROR;
          break;
        case Warning:
          result = IMarker.SEVERITY_WARNING;
          break;
        case Info:
          result = IMarker.SEVERITY_INFO;
          break;
      }
    } catch( final IllegalArgumentException illarex ) {
      // leave it to SEVERITY_INFO
    }
    return result;
  }


  private static int readInt( final String content ) {
    int result = UNSPECIFIED;
    if( content != null && content.trim().length() > 0 ) {
      try {
        result = Integer.parseInt( content );
      } catch( final NumberFormatException numfex ) {
        // leave it to UNSPECIFIED
      }
    }
    return result;
  }

  private List<IResource> findResource( final IPath path ) {
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    IFile[] files = root.findFilesForLocation( path );
    IContainer[] conts = root.findContainersForLocation( path );
    List<IResource> result = new ArrayList<IResource>();
    result.addAll( Arrays.asList( files ) );
    result.addAll( Arrays.asList( conts ) );
    return result;
  }
}
