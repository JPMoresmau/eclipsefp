// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;


public class SrcLoc {

  private final int line;
  private final int column;

  public static SrcLoc fromDocOffset(
      final IDocument doc,
      final int offset ) throws BadLocationException {
    int line = doc.getLineOfOffset( offset );
    int column = offset - doc.getLineOffset( line );
    return new SrcLoc( line, column );
  }

  public SrcLoc( final int line, final int column ) {
    this.line = line;
    this.column = column;
  }


  public int getLine() {
    return line;
  }

  public int getColumn() {
    return column;
  }
}
