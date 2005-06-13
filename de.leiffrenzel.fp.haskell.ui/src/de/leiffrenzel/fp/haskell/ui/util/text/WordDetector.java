// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.util.text;


import org.eclipse.jface.text.rules.IWordDetector;


/** <p>a word detector for Haskell identifiers.</p>
  * 
  * TODO: replace this by Haskell-aware word detecting
  * 
  * @author Leif Frenzel
  */
public class WordDetector implements IWordDetector {

  public boolean isWordPart( final char character ) {
    return Character.isJavaIdentifierPart( character );
  }
  
  public boolean isWordStart( final char character ) {
    return Character.isJavaIdentifierStart( character );
  }
}
