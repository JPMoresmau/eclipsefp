package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.Note;

/**
 * Parser that processes the GHC compiler output as it arrives and sends it off
 * in parsed form to an {@link IGhcOutputListener}.
 *
 * For parsing, the Parsec "grammar" from GHCOutputParser.hs is used.
 *
 * TODO TtC this should be an {@link ICompilerListener}, I think.
 *
 * @author Thomas ten Cate
 */
public class GhcOutputParser {

  private final BufferedReader reader;
  private String line;
  private int pos; // position within the line; only valid sometimes
  private final IGhcOutputListener listener;

  private final Pattern compilingPattern = Pattern
      .compile( "^(\\[(\\d+) of (\\d+)\\]\\s*)?Compiling\\s+\\S+\\s+\\(\\s*([^,]*),\\s+.*\\s*\\)$" ); //$NON-NLS-1$
  private static Pattern parenthesizedLocPattern = Pattern
      .compile( "^\\((\\d+):(\\d+)\\)-\\((\\d+):(\\d+)\\)" ); //$NON-NLS-1$
  private static Pattern sepLocsPattern = Pattern
      .compile( "^(\\d+):(\\d+)(-(\\d+))?" ); //$NON-NLS-1$

  public GhcOutputParser( final Reader reader, final IGhcOutputListener listener ) {
    this.reader = new BufferedReader( reader );
    this.listener = listener;
  }

  /**
   * Parses the input stream until EOF occurs.
   */
  public void parse() throws IOException {
    nextLine(); // initialize
    messages(); // main parsing method
  }

  // ///////////////////////
  // main parsing methods

  private void messages() throws IOException {
    while( !eof() ) {
      // abusing short-circuit ||
      if( !( compiling() || skipping() || ghcMessage() ) ) {
        // don't know what this is; be lenient
        nextLine();
      }
    }
  }

  /**
   * Either parses the current "[x of y] Compiling SomeFile.hs" line and returns
   * true, or consumes no input and returns false.
   */
  private boolean compiling() throws IOException {
    Matcher matcher = compilingPattern.matcher( line );
    if( !matcher.matches() ) {
      return false;
    }
    int number, total;
    String fileName;
    if( matcher.groupCount() == 4 ) {
      number = Integer.parseInt( matcher.group( 2 ) );
      total = Integer.parseInt( matcher.group( 3 ) );
      fileName = matcher.group( 4 );
    } else {
      // just guessing...
      number = 1;
      total = 1;
      fileName = matcher.group( 1 );
    }
    listener.compiling( fileName, number, total );
    nextLine();
    return true;
  }

  /**
   * Either parses the current "Skipping SomeFile.hs" line and returns true, or
   * consumes no input and returns false.
   */
  private boolean skipping() throws IOException {
    if( !line.startsWith( "Skipping " ) ) { //$NON-NLS-1$
      return false;
    }
    nextLine();
    return true;
  }

  /**
   * Either parses a compiler message or warning from the current line and
   * returns true, or consumes no input and returns false.
   */
  private boolean ghcMessage() throws IOException {
    pos = line.indexOf( ':' );
    if( pos < 0 ) {
      return false;
    }
    String fileName = line.substring( 0, pos );
    ++pos; // skip the colon

    Location location = location( fileName );
    if( location == null ) {
      return false;
    }
    ++pos; // skip the colon

    String message = line.substring( pos ).trim();
    if (message.length() == 0) {
      // sometimes the message is on its own line
      nextLine();
      message = line.trim();
    }
    nextLine();

    Note.Kind kind = message.startsWith( "Warning:" ) ? Note.Kind.WARNING : Note.Kind.ERROR; //$NON-NLS-1$
    if( kind == Note.Kind.WARNING ) {
      message = message.substring( 8 ).trim();
    }

    // Lines starting with four or more spaces form additional info
    StringBuffer additionalInfo = new StringBuffer();
    boolean first = true;
    while( !eof() && line.startsWith( "    " ) ) { //$NON-NLS-1$
      if( !first ) {
        additionalInfo.append( '\n' );
      }
      first = false;
      additionalInfo.append( line );
      nextLine();
    }

    Note note = new Note( kind, location, message, additionalInfo.toString() );
    listener.message( note );
    return true;
  }

  /**
   * Returns the location represented by the start of the given string. Returns
   * null if it could not be parsed.
   */
  private Location location( final String fileName ) {
    Location location = parenthesizedLoc( fileName );
    if( location != null ) {
      return location;
    }
    location = sepLocs( fileName );
    if( location != null ) {
      return location;
    }
    return null;
  }

  /**
   * Parses a location of the form (12,5)-(13,20) and returns it. Assumes that
   * {@link #pos} is valid. If parsing fails, returns null and leaves
   * {@link #pos} alone.
   */
  private Location parenthesizedLoc( final String fileName ) {
    Matcher matcher = parenthesizedLocPattern.matcher( line.substring( pos ) );
    if( matcher.lookingAt() ) {
      pos += matcher.end();
      try {
        int startLine = Integer.parseInt( matcher.group( 1 ) ) - 1;
        int startCol = Integer.parseInt( matcher.group( 2 ) );
        int endLine = Integer.parseInt( matcher.group( 3 ) ) - 1;
        int endCol = Integer.parseInt( matcher.group( 4 ) );
        return new Location( fileName, startLine, startCol, endLine, endCol );
      } catch( NumberFormatException ex ) {
        return null;
      }
    }
    return null;
  }

  /**
   * Parses a location of the form 12:5 or 12:5-20 and returns it. Assumes that
   * {@link #pos} is valid. If parsing fails, returns null and leaves
   * {@link #pos} alone.
   */
  private Location sepLocs( final String fileName ) {
    Matcher matcher = sepLocsPattern.matcher( line.substring( pos ) );
    if( matcher.lookingAt() ) {
      pos += matcher.end();
      if( matcher.group(4) == null ) {
        // 12:5
        try {
          int line = Integer.parseInt( matcher.group( 1 ) ) - 1;
          int col = Integer.parseInt( matcher.group( 2 ) );
          return new Location( fileName, line, col, line, col );
        } catch( NumberFormatException ex ) {
          return null;
        }
      } else if( matcher.groupCount() == 4 ) {
        // 12:5-20
        try {
          int line = Integer.parseInt( matcher.group( 1 ) ) - 1;
          int startCol = Integer.parseInt( matcher.group( 2 ) );
          int endCol = Integer.parseInt( matcher.group( 4 ) );
          return new Location( fileName, line, startCol, line, endCol );
        } catch( NumberFormatException ex ) {
          return null;
        }
      }
    }
    return null;
  }

  // //////////////////
  // parsing helpers

  private void nextLine() throws IOException {
    line = reader.readLine();
    pos = 0;
  }

  private boolean eof() {
    return line == null;
  }

}
