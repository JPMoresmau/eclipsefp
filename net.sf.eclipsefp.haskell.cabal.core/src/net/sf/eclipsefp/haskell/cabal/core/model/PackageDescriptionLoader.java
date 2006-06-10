// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.core.model;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.sf.eclipsefp.haskell.cabal.core.CabalSyntax;
import net.sf.eclipsefp.haskell.cabal.core.internal.CabalCorePlugin;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/** <p>contains helping functionality for loading a {@link PackageDescription 
  * PackageDescription model}.</p> 
  *
  * @author Leif Frenzel
  */
public class PackageDescriptionLoader {

  public static PackageDescription load( final String content ) {
    PackageDescription result = new PackageDescription();
    if( content != null && content.trim().length() > 0 ) {
      try {
        parse( content, result );
      } catch( final IOException ioex ) {
        // very unlikely
        log( ioex );
      }
    }
    return result;
  }


  // helping methods
  //////////////////
  
  private static void parse( final String content, 
                             final PackageDescription pd ) throws IOException {
    List<StanzaInfo> stanzas = new ArrayList<StanzaInfo>();
    StanzaInfo lastStanza = new StanzaInfo();
    stanzas.add( lastStanza );
    
    BufferedReader br = new BufferedReader( new StringReader( content ) );
    int count = 0;
    boolean contentStarted = false;
    String line = br.readLine();
    while( line != null ) {
      count++;
      if( !isComment( line ) ) {
        contentStarted = true;
        if( isEmpty( line ) ) {
          lastStanza.setEnd( count - 1 );
          lastStanza = new StanzaInfo();
          lastStanza.setStart( count );
          stanzas.add( lastStanza );
        } else {
          lastStanza.getContent().add( line );
        }
      } else if( !contentStarted ) {
        lastStanza.setStart( lastStanza.getStart() + 1 );
      }
      line = br.readLine();
    }
    if( !lastStanza.getContent().isEmpty() ) {
      // then we had not yet a chance to set the end line
      lastStanza.setEnd( count );
    }
    applyStanzas( stanzas, pd );
  }

  private static void applyStanzas( final List<StanzaInfo> stanzas, 
                                    final PackageDescription pd ) {
    Iterator<StanzaInfo> it = stanzas.iterator();
    while( it.hasNext() ) {
      StanzaInfo info = it.next();
      if( !info.getContent().isEmpty() ) {
        pd.addStanza( create( info.getStart(), 
                              info.getEnd(), 
                              info.getContent() ) );
      }
    }
  }

  private static PackageDescriptionStanza create( final int start, 
                                                  final int end, 
                                                  final List<String> content ) {
    PackageDescriptionStanza result;
    if( isExecutable( content ) ) {
      result = new ExecutableStanza( getExecutableName( content ), start, end );
    } else if( isLibrary( content ) ) {
      result = new LibraryStanza( getLibraryName( content ), start, end );
    } else {
      result = new GeneralStanza( getPlainName( content ), start, end );
    }
    return result;
  }

  private static String getPlainName( final List<String> content ) {
    String result = getValue( content, CabalSyntax.FIELD_NAME );
    if( result == null ) {
      result = "Unknown";
    }
    return result;
  }

  private static String getLibraryName( final List<String> content ) {
    return getValue( content, CabalSyntax.FIELD_NAME );
  }

  private static String getExecutableName( final List<String> content ) {
    return getValue( content, CabalSyntax.FIELD_EXECUTABLE );
  }

  private static String getValue( final List<String> content, 
                                  final String fieldName ) {
    String result = null;
    Iterator<String> it = content.iterator();
    while( result == null && it.hasNext() ) {
      String next = it.next();
      String nextNoCase = next.trim().toLowerCase();
      if( nextNoCase.startsWith( fieldName.toLowerCase() ) ) {
        int index = next.indexOf( ':' ) + 1;
        result = next.substring( index ).trim();
      }
    }
    return result;
  }

  private static boolean isLibrary( final List<String> content ) {
    return containsLineStart( content, CabalSyntax.FIELD_EXPOSED_MODULES );
  }

  private static boolean isExecutable( final List<String> content ) {
    return containsLineStart( content, CabalSyntax.FIELD_EXECUTABLE );
  }

  private static boolean containsLineStart( final List<String> content, 
                                            final String start ) {
    boolean result = false;
    Iterator<String> it = content.iterator();
    while( !result && it.hasNext() ) {
      String next = it.next().trim().toLowerCase();
      result = next.startsWith( start.toLowerCase() );
    }
    return result;
  }

  private static boolean isEmpty( final String line ) {
    return line.trim().length() == 0;
  }

  private static boolean isComment( final String line ) {
    return line.startsWith( "--" ); //$NON-NLS-1$
  }
  
  private static void log( final IOException ex ) {
    String msg = ex.getMessage() == null ? "[No details]"  //$NON-NLS-1$
                                         : ex.getMessage();
    String pluginId = CabalCorePlugin.getPluginId();
    IStatus status = new Status( IStatus.ERROR, pluginId, 0, msg, ex );
    CabalCorePlugin.getDefault().getLog().log( status );
  }
  
  
  // inner classes
  ////////////////
  
  private static class StanzaInfo {
    
    private final List<String> content;
    private int start = 0;
    private int end = 1;
    
    StanzaInfo() {
      this.content = new ArrayList<String>();
    }

    List<String> getContent() {
      return content;
    }

    void setEnd( final int end ) {
      this.end = end;
    }

    int getEnd() {
      return end;
    }

    void setStart( final int start ) {
      this.start = start;
    }
    
    int getStart() {
      return start;
    }
  }
}
