package net.sf.eclipsefp.haskell.core.parser;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;

/**
 * Small utilities for simple parsing tasks
 * @author JP Moresmau
 *
 */
public class ParserUtils {

  /**
   *
   * @param line
   * @param offset
   * @return the word region, or null if no word was found
   */
  public static IRegion getHaskellWordRegion(final String line,final int offset){
    if (offset<0 || offset>=line.length()){
      return null; // TODO should probably be an exception
    }
    int start=offset;
    while (start>=0){
      char c=line.charAt( start );
      if (!isHaskellWordChar(c)){
        break;
      }
      start--;
    }
    if (offset == start) {
      return null;
    }
    start += 1;
    int end = offset+1;
    while (end<line.length()){
      char c=line.charAt( end );
      if (!isHaskellWordChar(c)){
        break;
      }
      end++;
    }
    end -= 1;
    return new Region( start, end-start+1 );
  }

  /**
   *
   * @param line
   * @param offset
   * @return the word, or null if none found
   */
  public static String getHaskellWord(final String line,final int offset){
    IRegion region = getHaskellWordRegion(line, offset);
    if (region == null) {
      return null;
    }
    return line.substring( region.getOffset(), region.getLength()+region.getOffset() );
  }

  private static boolean isHaskellWordChar(final char c){
    return (Character.isLetter( c ) || Character.isDigit( c ) || c=='_' || c=='\'' || c=='.');
  }

}
