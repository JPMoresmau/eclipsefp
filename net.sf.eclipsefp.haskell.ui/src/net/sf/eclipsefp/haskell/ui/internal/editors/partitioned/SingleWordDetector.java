package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import org.eclipse.jface.text.rules.IWordDetector;


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
