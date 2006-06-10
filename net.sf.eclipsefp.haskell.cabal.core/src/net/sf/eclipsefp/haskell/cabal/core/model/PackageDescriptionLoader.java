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
    List<List<String>> stanzas = new ArrayList<List<String>>();
    List<String> lastStanza = new ArrayList<String>();
    stanzas.add( lastStanza );
    
    BufferedReader br = new BufferedReader( new StringReader( content ) );
    String line = br.readLine();
    while( line != null ) {
      if( !isComment( line ) ) {
        if( isEmpty( line ) ) {
          lastStanza = new ArrayList<String>();
          stanzas.add( lastStanza );
        } else {
          lastStanza.add( line );
        }
      }
      line = br.readLine();
    }
    
    applyStanzas( stanzas, pd );
  }

  private static void applyStanzas( final List<List<String>> stanzas, 
                                    final PackageDescription pd ) {
    Iterator<List<String>> it = stanzas.iterator();
    while( it.hasNext() ) {
      List<String> stanza = it.next();
      if( !stanza.isEmpty() ) {
        pd.addStanza( createStanza( stanza ) );
      }
    }
  }

  private static PackageDescriptionStanza createStanza( final List<String> content ) {
    PackageDescriptionStanza result;
    if( isExecutable( content ) ) {
      result = new ExecutableStanza( getExecutableName( content ) );
    } else if( isLibrary( content ) ) {
      result = new LibraryStanza( getLibraryName( content ) );
    } else {
      result = new GeneralStanza( getPlainName( content ) );
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
}
