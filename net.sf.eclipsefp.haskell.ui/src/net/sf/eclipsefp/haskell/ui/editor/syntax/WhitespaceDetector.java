// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.syntax;

import org.eclipse.jface.text.rules.IWhitespaceDetector;

/** <p>determines whether a given character is a whitespace.</p>
 * 
 * @author Leif Frenzel
 */
public class WhitespaceDetector implements IWhitespaceDetector {

  public boolean isWhitespace( final char c ) {
    return ( ( c == ' ' ) || ( c == '\n' ) || ( c == '\r' ) );
  }
}
