package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.types.Location;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Superclass for TokenPrecedingPoint and TokenAtPoint, since they share the same
 * parameters. Only the command name differs.
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public abstract class TokenRelativeToPoint extends ScionCommand {
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
  /** The lexer token's location in the source, as reported by the scion-server */
  Location tokenLocation;
  
  /** The usual constructor
   * 
   * @param contents The current document's contents
   * @param line The current line of the editor's point
   * @param column The current column of the editor's point
   * @param literate Literate Haskell source flag
   */
  public TokenRelativeToPoint(String contents, Location editPoint, boolean literate) {
    super();
    this.theDocument = contents;
    this.editPoint = editPoint;
    this.literate = literate;
    
    this.token = null;
    this.tokenLocation = null;
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
        
        int startLine = result.getInt(1);
        int startColumn = result.getInt(2);
        int endLine = result.getInt(3);
        int endColumn = result.getInt(4);
        
        tokenLocation = new Location(new String(), startLine, startColumn, endLine, endColumn);
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
   * Get the token's Location
   */
  public Location getTokenLocation() {
    return tokenLocation;
  }
}
