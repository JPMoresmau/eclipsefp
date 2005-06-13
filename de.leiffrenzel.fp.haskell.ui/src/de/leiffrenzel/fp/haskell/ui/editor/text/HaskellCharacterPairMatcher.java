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

  public IRegion match( final IDocument document, final int offset ) {
    Assert.isNotNull( document );
    Assert.isLegal( offset >= 0 );
    this.document = document;
    this.offset = offset;

    IRegion retVal = null;
    try {
      retVal = matchPairsAt();
    } catch( BadLocationException e ) {
      // ignore, there's probably no matching character to highlight
    }
    return retVal;
  }

  public int getAnchor() {
    return anchor;
  }

  
  // helping methods
  //////////////////

  private boolean isClosingCharacter( final char ch ) {
    boolean result = false;
    for( int i = 1; !result && i < PAIRS.length; i += 2 ) {
      if( ch == PAIRS[ i ] ) {
        result = true;
      }
    }
    return result;
  }

  private boolean isOpeningCharacter( final char ch ) {
    boolean result = false;
    for( int i = 0; !result && i < PAIRS.length; i += 2 ) {
      if( ch == PAIRS[ i ] ) {
        result = true;
      }
    }
    return result;
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