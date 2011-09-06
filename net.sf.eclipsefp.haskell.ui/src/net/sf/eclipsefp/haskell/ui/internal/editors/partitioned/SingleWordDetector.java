/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import org.eclipse.jface.text.rules.IWordDetector;

/**
 * Detector of single words, that is, set of characters
 * separated from the rest by space characters.
 * @author Alejandro Serrano
 *
 */
public class SingleWordDetector implements IWordDetector {

  private final String string;

  public SingleWordDetector(final String string) {
    this.string = string;
  }

  public boolean isWordStart( final char c ) {
    return c == string.charAt( 0 );
  }

  public boolean isWordPart( final char c ) {
    return string.indexOf( c, 1 ) != -1;
  }

}
