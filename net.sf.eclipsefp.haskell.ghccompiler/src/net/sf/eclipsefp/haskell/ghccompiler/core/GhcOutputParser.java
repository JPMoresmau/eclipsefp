// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.Note;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;

/**
 * Parses GHC compilation output messages and turns them into {@link Note}
 * objects that can be applied to resources to indicate errors and warnings.
 *
 * This is an ugly port of GHCOutputParser.hs.
 *
 * @author Thomas ten Cate
 */
public class GhcOutputParser {

  public static void applyOutput( final String output, final File workDir ) {
    for( Note note: new GhcOutputParser( output ).parse() ) {
      // TODO files that get compiled as dependencies of the current file
      // do receive their markers, but old markers aren't cleared from those.
      try {
        // The returned file names are relative to the working directory
        File absoluteFileName = new File(workDir, note.getLocation().getFileName());
        IFile file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation( new Path(absoluteFileName.getCanonicalPath()) );
        note.applyAsMarker( file );
      } catch( CoreException ex ) {
        // well then, no marker
      } catch( IOException ex ) {
        // getCanonicalPath failed (file does not exist?)
      }
    }
  }

  private static Pattern counterPat = Pattern.compile( "^\\[\\d+ of \\d+\\]" ); //$NON-NLS-1$
  private static Pattern parenthesizedLocPat = Pattern
      .compile( "^\\((\\d+):(\\d+)\\)-\\((\\d+):(\\d+)\\)" ); //$NON-NLS-1$
  private static Pattern sepLocsPat = Pattern
      .compile( "^(\\d+):(\\d+)(-(\\d+))?" ); //$NON-NLS-1$

  private final String output;
  private String[] lines;
  private int curLine, numLines;
  private int curCol, numCols;
  private List<Note> notes;

  private GhcOutputParser( final String output ) {
    this.output = output;
    this.notes = null;
  }

  private List<Note> parse() {
    if( notes == null ) {
      notes = new ArrayList<Note>();
      lines = output.split( "\\n" ); //$NON-NLS-1$
      numLines = lines.length;
      curLine = curCol = 0;
      ignoreLine( "Chasing modules from: " ); //$NON-NLS-1$
      ignoreStuff();
      Note note;
      do {
        try {
          note = parseMessage();
        } catch( IndexOutOfBoundsException ex ) {
          // give up
          break;
        }
        if( note != null ) {
          notes.add( note );
        }
      } while( note != null );
      ignoreLine( "Linking " ); //$NON-NLS-1$
    }
    return notes;
  }

  private Note parseMessage() {
    skipWhitespace();
    int fileNameEnd = curLine().indexOf( ':', curCol );
    String fileName = curLine().substring( curCol, fileNameEnd );

    curCol = fileNameEnd + 1;
    Location location = parenthesizedLoc( fileName );
    if (location == null) {
      location = sepLocs( fileName );
    }
    if (location == null) {
      return null;
    }

    nextCol();
    skipWhitespace();

    String message = curLine().substring( curCol ).trim();
    boolean isWarning = message.startsWith( "Warning:" ); //$NON-NLS-1$
    if( isWarning ) {
      message = message.substring( 8 ).trim();
    }

    return new Note( isWarning ? Note.Kind.WARNING : Note.Kind.ERROR, location,
        message );
  }

  /**
   * (12,5)-(13,20)
   */
  private Location parenthesizedLoc( final String fileName ) {
    Matcher matcher = parenthesizedLocPat.matcher( curLine().substring( curCol ) );
    if (matcher.lookingAt()) {
      curCol += matcher.end();
      return new Location( fileName, Integer.parseInt( matcher
          .group( 1 ) ) - 1, Integer.parseInt( matcher.group( 2 ) ), Integer
          .parseInt( matcher.group( 3 ) ) - 1, Integer.parseInt( matcher.group( 4 ) ) );
    }
    return null;
  }

  /**
   * 12:5 or 12:5-20
   */
  private Location sepLocs( final String fileName ) {
    Matcher matcher = sepLocsPat.matcher( curLine().substring( curCol ) );
    if (matcher.lookingAt()) {
      curCol += matcher.end();
    }
    if( matcher.groupCount() == 2 ) {
      // 12:5
      return new Location( fileName,
          Integer.parseInt( matcher.group( 1 ) ) - 1, Integer.parseInt( matcher
              .group( 2 ) ), Integer.parseInt( matcher.group( 1 ) ) - 1, Integer
              .parseInt( matcher.group( 2 ) ) );
    } else if (matcher.groupCount() == 4 ) {
      // 12:5-20
      return new Location( fileName,
          Integer.parseInt( matcher.group( 1 ) ) - 1, Integer.parseInt( matcher
              .group( 2 ) ), Integer.parseInt( matcher.group( 1 ) ) - 1, Integer
              .parseInt( matcher.group( 4 ) ) );
    }
    return null;
  }

  private void skipWhitespace() {
    while( !eof() && " \t\r\n".indexOf( curChar() ) >= 0 ) { //$NON-NLS-1$
      nextCol();
    }
  }

  private void nextCol() {
    ++curCol;
    if (curCol >= numCols) {
      ++curLine;
      curCol = 0;
      numCols = (eof() ? 0 : curLine().length());
    }
  }

  private String curLine() {
    return lines[curLine];
  }

  private char curChar() {
    if (curCol >= numCols) {
      return '\n';
    }
    return curLine().charAt( curCol );
  }

  private boolean eof() {
    return curLine >= numLines;
  }

  private void ignoreLine( final String start ) {
    // assumes that we're at the beginning of the line
    while( !eof() && lines[ curLine ].startsWith( start ) ) {
      ++curLine;
    }
    curCol = 0;
  }

  private void ignoreStuff() {
    // assumes that we're at the beginning of the line
    while( !eof()
        && ( lines[ curLine ].startsWith( "Compiling " ) || //$NON-NLS-1$
            stripCounter( lines[ curLine ] ).startsWith( " Compiling " ) || //$NON-NLS-1$
        lines[ curLine ].startsWith( "Skipping " ) ) ) { //$NON-NLS-1$
      ++curLine;
    }
    curCol = 0;
  }

  private String stripCounter( final String line ) {
    Matcher matcher = counterPat.matcher( line );
    if( matcher.lookingAt() ) {
      return line.substring( matcher.end() );
    }
    return line;
  }

}
