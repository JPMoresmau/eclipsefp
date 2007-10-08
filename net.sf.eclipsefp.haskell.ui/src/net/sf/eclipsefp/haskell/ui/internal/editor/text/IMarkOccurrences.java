// Copyright (c) 2007 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.editor.text;


/** <p>implementors know how to compute occurrences of the identifier (if any)
  * at a cursor position in a Haskell source text.</p> 
  *
  * @author Leif Frenzel
  */
public interface IMarkOccurrences {

  Occurrence[] mark( final String bufferContent, 
                     final int cursorLine, 
                     final int cursorColumn );
}
