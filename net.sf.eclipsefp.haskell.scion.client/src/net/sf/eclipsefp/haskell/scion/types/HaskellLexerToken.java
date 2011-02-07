package net.sf.eclipsefp.haskell.scion.types;


/**
 * The Haskell token lexer container class.
 *
 * @author B. Scott Michel (bscottm@ieee.org)
 */
public class HaskellLexerToken {
  /** The token, itself */
  private final String token;
  /** The location where the token occurs in the source */
  private final Location tokenLoc;

  public HaskellLexerToken(final String token, final int startLine, final int startColumn, final int endLine, final int endColumn) {
    this.token = token;
    this.tokenLoc = new Location("", startLine, startColumn, endLine, endColumn); //$NON-NLS-1$
  }

  public String getToken() {
    return token;
  }

  public Location getTokenLoc() {
    return tokenLoc;
  }
}
