// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.util.text;

import java.io.IOException;

import org.eclipse.jface.text.IDocument;


/** <p>provides functionality to match characters with their counterparts in
  * a document.</p>
  * 
  * @author Leif Frenzel
  */
public class Matcher {

  public static int findClosing( final IDocument document, 
                                 final int offset,
                                 final char opening ) {
    int result = -1;
    try {
      CodeReader reader = new ForwardCodeReader( document, offset );
      result = find( opening, reader );
    } catch( IOException ioex ) {
      // ignored
    }
    return result;
  }

  public static int findOpening( final IDocument document, 
                                 final int offset,
                                 final char closing ) {
    int result = -1;
    try {
      CodeReader reader = new BackwardCodeReader( document, offset + 1 );
      result = find( closing, reader );
    } catch( IOException ioex ) {
      // ignored
    }
    return result;
  }
  
  
  // helping methods
  //////////////////

  private static int find( final char charToMatch, 
                           final CodeReader reader ) throws IOException {
    char matchingChar = getPeerCharacter( charToMatch );
    int result = -1;
    int stack = 1;
    int c = reader.read();
    boolean finished = false;
    
    while( !finished && c != -1 ) {
      if( ( c == charToMatch ) && ( c != matchingChar ) ) {
        stack++;
      } else if( c == matchingChar ) {
        stack--;
      }
      if( stack == 0 ) {
        result = reader.getOffset();
        finished = true;
      }
      c = reader.read();
    }
    return result;
  }
  
  private static char getPeerCharacter( final char chr ) {
    char result = 0;
    switch( chr ) {
      case '{':
        result = '}';
      case '}':
        result = '{';
      case '[':
        result = ']';
      case ']':
        result = '[';
      case '(':
        result = ')';
      case ')':
        result = '(';
    }
    return result;
  }
}