package net.sf.eclipsefp.haskell.util;

/**
 * Haskell text utilities: identifier identification, etc.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */

public final class HaskellText {
  /**
   * Predicate for identifying part of a Haskell identifier, i.e., conid or varid in the
   * Haskell98 specification.
   * 
   * @param ch The character within the identifier part
   * @return true if the character is part of a Haskell identifier
   */
  public static boolean isHaskellIdentifierPart( char ch ) {
    return (   Character.getType(ch) == Character.UPPERCASE_LETTER
            || Character.getType(ch) == Character.LOWERCASE_LETTER
            || (ch == '_')
            || (ch == '\'') ); 
  }
  
  /**
   * Predicate for comment characters
   */
  public static boolean isCommentPart( char ch ) {
    return (   ch == '{'
            || ch == '-'
            || ch == '}');
  }
 
  /**
   * Predicate for symbol characters
   */
  public static boolean isSymbol( final char ch ) {
    return (   Character.getType( ch ) == Character.MATH_SYMBOL
            || Character.getType( ch ) == Character.CURRENCY_SYMBOL
            || Character.getType( ch ) == Character.MODIFIER_SYMBOL
            || Character.getType( ch ) == Character.OTHER_SYMBOL);
  }
}
