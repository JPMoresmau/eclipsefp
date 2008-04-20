// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.ICharacterPairMatcher;

/** <p>The pair matcher implementation used for matching parentheses etc.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellCharacterPairMatcher implements ICharacterPairMatcher {

  private static final int FORWARD = +1;
  private static final int BACKWARDS = -1;

  private static final char PAIRS[] = { '{', '}', '(', ')', '[', ']' };

  private int anchor;


  // interface methods of ICharacterPairMatcher
  /////////////////////////////////////////////

  public void clear() {
    anchor = 0;
  }

  public void dispose() {
    //nothing to dispose
  }

  public IRegion match(final IDocument document, final int theOffset) {
    Assert.isNotNull( document );
    Assert.isLegal( theOffset >= 0 );

    //special case, there is nothing to complete if cursor is on position zero
    if (0 == theOffset) {
      return null;
    }

    try {
      final int initialOffset = theOffset - 1;
      final char start = document.getChar(initialOffset);
      int direction = 0;
      if (isClosingCharacter(start)) {
        anchor = RIGHT;
        direction = BACKWARDS;
      } else if (isOpeningCharacter(start)) {
        anchor = LEFT;
        direction = FORWARD;
      } else {
        return null;
      }

      return matchPair(document, initialOffset, direction);
    } catch( BadLocationException ex ) {
      // ignore: probably there isn't a matching character yet
    }

    return null;
  }

  private IRegion matchPair(final IDocument document,
                            final int initialOffset,
                            final int searchDirection)
    throws BadLocationException
  {
    final Reader reader = new Reader(document, initialOffset, searchDirection);
    final char start = reader.readChar();
    final char match = pairFor(start);

    int length = 1;
    int nestingLevel = 0;
    //advance the starting character
    while(!reader.eof()) {
      char c = reader.readChar();
      ++length;
      if (c == match) {
        if (nestingLevel == 0) {
          return new Region(reader.getSmallestOffset(), length);
        }

        --nestingLevel;
      } else if (c == start){
        ++nestingLevel;
      }
    }

    return null;
  }

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

  private char pairFor(final char c) {
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

  private static class Reader {

    private final IDocument fSourceDoc;
    private final int fInitialOffset;
    private final int fIncrement;
    private int fLastOffset;

    public Reader(final IDocument document, final int initialOffset, final int increment) {
      fSourceDoc = document;
      fInitialOffset = initialOffset;
      fIncrement = increment;
      fLastOffset = fInitialOffset - fIncrement;
    }

    public int getSmallestOffset() {
      return Math.min(fInitialOffset, fLastOffset);
    }

    public char readChar() throws BadLocationException {
      fLastOffset += fIncrement;
      char result = fSourceDoc.getChar(fLastOffset);
      return result;
    }

    public boolean eof() {
      return (fIncrement < 0) ? fLastOffset < 0 : fLastOffset >= fSourceDoc.getLength();
    }
  }
}