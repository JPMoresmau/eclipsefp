// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.editor.text;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.util.Assert;

import de.leiffrenzel.fp.haskell.ui.util.text.Matcher;


/** <p>The pair matcher implementation used for matching parentheses etc.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellCharacterPairMatcher implements ICharacterPairMatcher {

  private static final char PAIRS[] = { '{', '}', '(', ')', '[', ']' };

  private IDocument document;
  private int offset;
  private int anchor;

  
  // interface methods of ICharacterPairMatcher
  /////////////////////////////////////////////

  public void clear() {
    document = null;
    offset = -1;
    anchor = 0;
  }

  public void dispose() {
    document = null;
  }

  public IRegion match(final IDocument document, final int theOffset) {
    Assert.isNotNull( document );
    Assert.isLegal( theOffset >= 1 );
    
    try {
      final char start = document.getChar(theOffset - 1);
      if (isClosingCharacter(start)) {
        final char match = pairFor(start);
        int offset = theOffset;
        while(offset > 0) {
          char c = document.getChar(offset);
          if (c == match) {
            return new Region(offset, theOffset - offset);
          }
          --offset;
        }
      } else if (isOpeningCharacter(start)) {
        final char match = pairFor(start);
        int offset = theOffset;
        while(offset < document.getLength()) {
          char c = document.getChar(offset);
          if (c == match) {
            return new Region(theOffset - 1, offset - theOffset + 2);
          }
          ++offset;
        }
      }
    } catch( BadLocationException ex ) {
      // ignore probably there isn't a matching character yet
    }
    
    return null;
  }

  //TODO remove this method
  public int getAnchor() {
    return anchor;
  }

  
  // helping methods
  //////////////////

  private boolean isClosingCharacter( final char ch ) {
    for( int i = 1; i < PAIRS.length; i += 2 ) {
      if( ch == PAIRS[ i ] ) {
        return true;
      }
    }
    return false;
  }

  private boolean isOpeningCharacter( final char ch ) {
    for( int i = 0; i < PAIRS.length; i += 2 ) {
      if( ch == PAIRS[ i ] ) {
        return true;
      }
    }
    return false;
  }

  private char pairFor(char c) {
    for(int i = 0; i < PAIRS.length; ++i ) {
      if (PAIRS[i] == c) {
        if (i % 2 == 0) {
          return PAIRS[i + 1];
        }
        
        return PAIRS[i - 1];
      }
    }
    return c;
  }

  private IRegion matchPairsAt() throws BadLocationException {
    int startPos = -1;
    int endPos = -1;
    IRegion result = null;
    
    char prevChar = document.getChar( Math.max( offset - 1, 0 ) );
    if( isOpeningCharacter( prevChar ) ) {
      startPos = offset - 1;
      if( startPos >= 0 ) {
        anchor = LEFT;
        endPos = Matcher.findClosing( document, startPos + 1, prevChar );
        if( endPos > -1 ) {
          result = new Region( startPos, endPos - startPos + 1 );
        }
      }
    }
    if( isClosingCharacter( prevChar ) ) {
      endPos = offset - 1;
      if( endPos >= 0 ) {
        anchor = RIGHT;
        startPos = Matcher.findOpening( document, endPos - 1, prevChar );
        if( startPos > -1 ) {
          result = new Region( startPos, endPos - startPos + 1 );
        }
      }
    }
    return result;
  }
}