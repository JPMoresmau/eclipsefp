// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.syntax;

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
