package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import org.eclipse.jface.text.rules.IWordDetector;


public class KeywordDetector implements IWordDetector {

  public static KeywordDetector NO_DIGIT_AT_START_DETECTOR = new KeywordDetector(false);
  public static KeywordDetector ALLOW_DIGIT_AT_START_DETECTOR = new KeywordDetector(true);

  boolean allowDigitAtStart;

  public KeywordDetector( final boolean allowDigitAtStart ) {
    this.allowDigitAtStart = allowDigitAtStart;
  }

  public boolean isWordStart( final char c ) {
    if( allowDigitAtStart ) {
      return Character.isLetter( c ) || Character.isDigit( c ) || c == '_';
    } else {
      return Character.isLetter( c ) || c == '_';
    }
  }

  public boolean isWordPart( final char c ) {
    return Character.isLetter( c ) || Character.isDigit( c ) || c == '_';
  }

}
