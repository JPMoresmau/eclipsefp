package net.sf.eclipsefp.haskell.core.parser;

/**
 * Small utilities for simple parsing tasks
 * @author JP Moresmau
 *
 */
public class ParserUtils {

  public static String getHaskellWord(final String line,final int offset){
    if (offset<0 || offset>=line.length()){
      return ""; //$NON-NLS-1$
    }
    StringBuilder sb=new StringBuilder();
    int st=offset;
    while (st>=0){
      char c=line.charAt( st );
      if (isHaskellWordChar(c)){
        sb.insert( 0, c );
      } else {
        break;
      }
      st--;
    }
    st=offset+1;
    while (st<line.length()){
      char c=line.charAt( st );
      if (isHaskellWordChar(c)){
        sb.append( c );
      } else {
        break;
      }
      st++;
    }
    return sb.toString();
  }

  private static boolean isHaskellWordChar(final char c){
    return (Character.isLetter( c ) || Character.isDigit( c ) || c=='_' || c=='\'' || c=='.');
  }

}
