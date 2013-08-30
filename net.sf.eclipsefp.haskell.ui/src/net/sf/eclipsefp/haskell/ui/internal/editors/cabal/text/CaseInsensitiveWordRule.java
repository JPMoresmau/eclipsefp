// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text;

import java.util.Iterator;
import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WordRule;

/** <p>a modified WordRule that also detects words if they are in different
  * case.</p>
  *
  * @author Leif Frenzel
  */
public class CaseInsensitiveWordRule extends WordRule {

  public CaseInsensitiveWordRule( final IWordDetector detector ) {
    super( detector );
  }

  public CaseInsensitiveWordRule( final IWordDetector detector,
                                  final IToken defaultToken ) {
    super( detector, defaultToken );
  }

  @Override
  public IToken evaluate( final ICharacterScanner scanner ) {
    // took the superclass functionality and accept now also words that have not the
    // exact same case as those in the map
    int c= scanner.read();
    if (c != ICharacterScanner.EOF && fDetector.isWordStart((char) c)) {
      if (fColumn == UNDEFINED || (fColumn == scanner.getColumn() - 1)) {
        while (c != ICharacterScanner.EOF && Character.isWhitespace( (char )c)){
          c= scanner.read();
        }

        if (fDetector.isWordStart((char) c)) {
          StringBuilder fBuffer = new StringBuilder();
          do {
            fBuffer.append((char) c);
            c= scanner.read();
          } while (c != ICharacterScanner.EOF && fDetector.isWordPart((char) c));
          scanner.unread();

          IToken token= findToken( fBuffer );
          if (token != null) {
            return token;
          }

          if (fDefaultToken.isUndefined()) {
            for (int i= fBuffer.length() - 1; i >= 0; i--) {
              scanner.unread();
            }
          }
          return fDefaultToken;
        }
      }
    }
    scanner.unread();
    return Token.UNDEFINED;
  }


  // helping methods
  //////////////////

  private IToken findToken( final StringBuilder sb ) {
    String content = sb.toString();
    IToken result = ( IToken )fWords.get( content );
    if( result == null ) {
      Iterator<?> it = fWords.keySet().iterator();
      while( result == null && it.hasNext() ) {
        String candidate = ( String )it.next();
        if( candidate.equalsIgnoreCase( content ) ) {
          result = ( IToken )fWords.get( candidate );
        }
      }
    }
    return result;
  }
}
