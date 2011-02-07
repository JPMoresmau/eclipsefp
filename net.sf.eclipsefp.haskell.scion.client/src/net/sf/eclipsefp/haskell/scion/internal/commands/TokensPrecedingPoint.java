package net.sf.eclipsefp.haskell.scion.internal.commands;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import net.sf.eclipsefp.haskell.scion.types.HaskellLexerToken;
import net.sf.eclipsefp.haskell.scion.types.Location;

/**
 * Get the token preceding the editor's point
 * 
 * @author B. Scott Michel (bscottm@ieee.org)
 *
 */
public class TokensPrecedingPoint extends TokenRelativeToPoint {
  /** Number of tokens requested that precede the edit point */
  private int numPreceding;
  
  /** The usual constructor
   * 
   * @param contents The current document's contents
   * @param line The current line of the editor's point
   * @param column The current column of the editor's point
   * @param literate Literate Haskell source flag
   */
  public TokensPrecedingPoint(final int numPreceding, final String contents, final Location editPoint, final boolean literate) {
    super(contents, editPoint, literate);
    this.numPreceding = numPreceding;
    lexTokens = null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMethod() {
    return "token-preceding";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected JSONObject getParams() throws JSONException {
    JSONObject params = super.getParams();
    params.put("numTokens", numPreceding);
    return params;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doProcessResult() throws JSONException {
    if (response instanceof JSONObject) {
      JSONObject o = (JSONObject) response;
      JSONArray result = o.optJSONArray("Right");
      if (result != null) {
        lexTokens = new HaskellLexerToken[result.length()];
        
        for (int i = 0; i < result.length(); ++i) {
          JSONArray thisTok = (JSONArray) result.get(i);
          String token = (String) thisTok.get(0);
          int startLine = thisTok.getInt(1);
          int startColumn = thisTok.getInt(2);
          int endLine = thisTok.getInt(3);
          int endColumn = thisTok.getInt(4);
          
          lexTokens[i] = new HaskellLexerToken(token, startLine, startColumn, endLine, endColumn);
        }
      }
    }
  }
}
