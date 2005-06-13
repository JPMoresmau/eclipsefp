// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.util.text;

import java.text.CharacterIterator;

import org.eclipse.jface.text.*;

/** <p>a character iterator that works on an <code>IDocument</code>.</p> 
  * 
  * <p>Taken from 
  * <code>org.eclipse.jface.text.DefaultTextDoubleClickStrategy</code>.</p>
  * 
  * @author Leif Frenzel
  */
class DocumentCharacterIterator implements CharacterIterator {
  
  /** Document to iterate over. */
  private IDocument document;
  /** Start offset of iteration. */
  private int offset= -1;
  /** End offset of iteration. */
  private int endOffset= -1;
  /** Current offset of iteration. */
  private int index= -1;
  
  
  void setDocument( final IDocument document, final IRegion iteratorRange ) {
    this.document = document;
    offset = iteratorRange.getOffset();
    endOffset = offset + iteratorRange.getLength();
  }
  
  
  // interface methods of CharacterIterator
  /////////////////////////////////////////
  
  public char first() {
    index = offset;
    return current();
  }
  
  public char last() {
    index = offset < endOffset ? endOffset - 1 : endOffset;
    return current();
  }
  
  public char current() {
    if( offset <= index && index < endOffset ) {
      try {
        return document.getChar( index );
      } catch( BadLocationException badlox ) {
        // ignored
      }
    }
    return DONE;
  }
  
  public char next() {
    char result = DONE;
    if( index != endOffset - 1 ) {
      if( index < endOffset ) {
        ++index;
      }
      result = current();
    }
    return result;
  }
  
  public char previous() {
    char result = DONE;
    if( index != offset ) {
      if (index > offset) {
        --index;
      }
      result = current();
    }
    return result;
  }
  
  public char setIndex( final int index ) {
    this.index = index;
    return current();
  }
  
  public int getBeginIndex() {
    return offset;
  }
  
  public int getEndIndex() {
    return endOffset;
  }
  
  public int getIndex() {
    return index;
  }
  
  public Object clone() {
    DocumentCharacterIterator result = new DocumentCharacterIterator();
    result.document = document;
    result.index = index;
    result.offset = offset;
    result.endOffset = endOffset;
    return result;
  }
}