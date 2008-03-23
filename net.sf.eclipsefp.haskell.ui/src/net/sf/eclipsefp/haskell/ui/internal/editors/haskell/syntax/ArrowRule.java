// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.syntax;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.jface.text.rules.WordRule;

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