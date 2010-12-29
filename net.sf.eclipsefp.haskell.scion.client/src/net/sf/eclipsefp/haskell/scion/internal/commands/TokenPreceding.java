package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.types.Location;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Get the token preceding the editor's point
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 *
 */
public class TokenPreceding extends ScionCommand {
  /** The current document's contents */
  String theDocument;
  /** The location of the editor's point */
  Location editPoint;
  /** The column within the line where the editor's point is located. */
  int column;
  /** Literate Haskell flag */
  boolean literate;
  /** The lexer token returned by scion-server, see {@link IHaskellTokens} */
  String token;
  
  /** The usual constructor
   * 
   * @param contents The current document's contents
   * @param line The current line of the editor's point
   * @param column The current column of the editor's point
   * @param literate Literate Haskell source flag
   */
  public TokenPreceding(String contents, Location editPoint, boolean literate) {
    super();
    this.theDocument = contents;
    this.editPoint = editPoint;
    this.literate = literate;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected JSONObject getParams() throws JSONException {
    JSONObject params = super.getParams();
    params.put("contents", theDocument);
    params.put("line", editPoint.getStartLine());
    params.put("column", editPoint.getStartColumn());
    params.put("literate", literate);
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
        token = (String) result.get(0);
      }
    }
  }
  
  /**
   * Get the Haskel lexer token string.
   */
  public String getTokenString() {
    return token;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMethod() {
    return "token-preceding";
  }
}
