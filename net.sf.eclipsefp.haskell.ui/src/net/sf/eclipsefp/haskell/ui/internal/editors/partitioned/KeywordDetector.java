/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import org.eclipse.jface.text.rules.IWordDetector;

/**
 * Detects keywords in a file.
 * @author Alejandro Serrano
 *
 */
public class KeywordDetector implements IWordDetector {

  public static KeywordDetector NO_DIGIT_AT_START_DETECTOR = new KeywordDetector(false);
  public static KeywordDetector ALLOW_DIGIT_AT_START_DETECTOR = new KeywordDetector(true);

  boolean allowDigitAtStart;

  public KeywordDetector( final boolean allowDigitAtStart ) {
    this.allowDigitAtStart = allowDigitAtStart;
  }

  @Override
  public boolean isWordStart( final char c ) {
    if( allowDigitAtStart ) {
      return Character.isLetter( c ) || Character.isDigit( c ) || c == '_';
    } else {
      return Character.isLetter( c ) || c == '_';
    }
  }

  @Override
  public boolean isWordPart( final char c ) {
    return Character.isLetter( c ) || Character.isDigit( c ) || c == '_';
  }

}
