// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util.text;

import java.text.BreakIterator;

import org.eclipse.jface.text.*;


/** <p>A helping class for detecting words in documents.</p>
  *
  * <p>Taken from 
  * <code>org.eclipse.jface.text.DefaultTextDoubleClickStrategy</code>.</p>
  *
  * @author Leif Frenzel
  */
public class WordFinder {
  
  private final DocumentCharacterIterator docIter = new DocumentCharacterIterator();
  
  public IRegion findWord( final IDocument document, final int position ) {
    IRegion result = null;
    if( position >= 0 ) {
      try {
        IRegion line = document.getLineInformationOfOffset( position );
        int lineEnd = line.getOffset() + line.getLength();
        if( position != lineEnd ) {
          BreakIterator breakIter = BreakIterator.getWordInstance();
          docIter.setDocument( document, line );
          breakIter.setText( docIter );
          int start = breakIter.preceding( position );
          if( start == BreakIterator.DONE ) {
            start = line.getOffset();
          }
          int end = breakIter.following( position );
          if( end == BreakIterator.DONE ) {
            end = lineEnd;
          }
          if( breakIter.isBoundary( position ) ) {
            if( end - position > position - start ) {
              start = position;
            } else {
              end = position;
            }
          }
          if( start != end ) {
            result = new Region( start, end - start );
          }
        }
      } catch( final BadLocationException badlox ) {
        // ignored
      }
    }
    return result;
  }
}
