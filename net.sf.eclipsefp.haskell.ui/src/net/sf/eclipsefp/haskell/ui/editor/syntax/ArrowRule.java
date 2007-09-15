// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.syntax;

import org.eclipse.jface.text.rules.*;

/** <p>A word predicate rule for the special case of the arrow keyword.</p>
 *  
 * @author Leif Frenzel
 */
public class ArrowRule extends WordRule implements IPredicateRule {
    
  private final IToken successToken;
    
  public ArrowRule( final IToken successToken ) {
    super( new IWordDetector() {
      public boolean isWordStart( final char c ) {
        return( c == '-' );
      }

      public boolean isWordPart( final char c ) {
        return( c == '-' || c == '>' );
      }      
    } );
    this.successToken = successToken;
    addWord( "->", successToken );
  }
    
  public IToken evaluate( final ICharacterScanner scanner, 
                          final boolean resume ) {
    return super.evaluate( scanner );
  }

  public IToken getSuccessToken() {
    return successToken;
  }
}