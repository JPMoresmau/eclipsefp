// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.util.text;

import java.io.IOException;
import java.io.Reader;

import org.eclipse.jface.text.IDocument;


/** <p>Reader implementation for Haskell source code from a Haskell editor
  * document.</p>
  * 
  * @author Leif Frenzel
  */
abstract class CodeReader extends Reader {

  // TODO jdt uses a code reader that can skip comments and string literals
  // --> have a look at that
  
  /** read the document backwards. */
  static final int BACKWARD = -1;
  /** read the document forwards. */
  static final int FORWARD = 1;
  
  
  /** The offset at which to start reading. */
  int offset;
  private IDocument document;

  
  CodeReader( final IDocument document, final int offset ) {
    this.document = document;
    this.offset = offset;
  }
  
  /**
    * Returns the offset of the last read character. Should only be called 
    * after read has been called.
    */
  abstract int getOffset();
  
  IDocument getDocument() {
    return this.document;
  }

  
  // interface methods of Reader
  //////////////////////////////
  
  public int read( final char[] cbuf, 
                   final int off, 
                   final int len ) throws IOException {
    int end = off + len;
    int result = len;
    boolean finished = false;
    for( int i = off; !finished && i < end; i++ ) {
      int ch = read();
      if( ch == -1 ) {
        result = -1;
        if( i != off ) {
          result = i - off;
        }
        finished = true;
      } else {
        cbuf[ i ] = ( char )ch;
      }
    }
    return result;
  }

  public boolean ready() throws IOException {
    return true;
  }

  public String getString() throws IOException {
    StringBuffer sbResult = new StringBuffer();
    int ch = read();
    while( ch != -1 ) {
      sbResult.append( ( char )ch );
      ch = read();
    }
    return sbResult.toString();
  }
  
  public void close() throws IOException {
    document = null;
  }
  
  
  // helping methods
  //////////////////
  
  void checkDocument() {
    if( this.document == null ) {
      throw new IllegalStateException();
    }
  }
}