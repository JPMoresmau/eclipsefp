// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util.text;

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
  
  @Override
  public char first() {
    index = offset;
    return current();
  }
  
  @Override
  public char last() {
    index = offset < endOffset ? endOffset - 1 : endOffset;
    return current();
  }
  
  @Override
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
  
  @Override
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
  
  @Override
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
  
  @Override
  public char setIndex( final int index ) {
    this.index = index;
    return current();
  }
  
  @Override
  public int getBeginIndex() {
    return offset;
  }
  
  @Override
  public int getEndIndex() {
    return endOffset;
  }
  
  @Override
  public int getIndex() {
    return index;
  }
  
  @Override
  public Object clone() {
    DocumentCharacterIterator result = new DocumentCharacterIterator();
    result.document = document;
    result.index = index;
    result.offset = offset;
    result.endOffset = endOffset;
    return result;
  }
}