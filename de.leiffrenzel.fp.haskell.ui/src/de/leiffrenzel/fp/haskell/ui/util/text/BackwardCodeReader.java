// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.util.text;

import java.io.IOException;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;



/** <p>a code reader that reads backwards.</p>
  * 
  * @author Leif Frenzel
  */
class BackwardCodeReader extends CodeReader {

  BackwardCodeReader( final IDocument document, 
                      final int offset ) {
    super( document, offset );
  }
  
  
  // overriden methods from Reader
  ////////////////////////////////
  
  public int read() throws IOException {
    checkDocument();
    int result;
    try {
      result = readBackwards();
    } catch( BadLocationException ble ) {
      throw new IOException( ble.getMessage() );
    }
    return result;
  }
  
  
  // interface methods of CodeReader
  //////////////////////////////////
  
  int getOffset() {
    return this.offset;
  }
  
  
  // helping methods
  //////////////////
  
  private int readBackwards() throws BadLocationException {
    int result = -1;
    if( this.offset > 0 ) {
      this.offset--;
      result = getDocument().getChar( this.offset );
    }
    return result;
  }
}