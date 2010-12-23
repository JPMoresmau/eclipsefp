package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.types.Location;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Command that returns information about the thing under the "point" (which is
 * Emacs-speak for "cursor").
 * 
 * @author Thomas ten Cate
 */
public class ThingAtPointCommand extends ScionCommand {
  /** The location of the "thing" */
  private Location location;
  /** The "thing", a String with the thing's type information or Haddock documentation */
  private String   thing;   // the response
  /** Flag for whether the "thing" should be fully qualified */
  private boolean  qualify;
  /** Flag for whether the "thing" is a type signature (true) or Haddock documentation (false) */
  private boolean  typed;

  /**
   * Constructs a ThingAtPoint command for an item of interest.
   * 
   * @param location The "thing"'s location in the source
   * @param qualify Flag for whether the "thing"'s result should be a fully qualified name
   * @param typed Flag for whether the "thing"'s result should be a type signature (true) or Haddock documentation (false)
   */
  public ThingAtPointCommand(Location location, boolean qualify, boolean typed) {
    super();
    this.location = location;
    this.qualify = qualify;
    this.typed = typed;
  }

  /** Accessor for the result */
  public String getThing() {
    return thing;
  }

  @Override
  public String getMethod() {
    return "thing-at-point";
  }

  @Override
  protected JSONObject getParams() throws JSONException {
    JSONObject params = new JSONObject();
    params.put("file", location.getFileName());
    params.put("line", location.getStartLine());
    params.put("column", location.getStartColumn());
    params.put("qualify", qualify);
    params.put("typed", typed);
    return params;
  }

  @Override
  protected void doProcessResult() throws JSONException {
    if (response instanceof String) {
      thing = (String) response;
    } else if (!JSONObject.NULL.equals(response)) {
      JSONObject result = (JSONObject) response;
      if (result.has("Just")) {
        thing = result.getString("Just");
      } else {
        thing = null;
      }
    }
    
    // No result -> convert to null (this is suboptimal)
    if ("no info".equalsIgnoreCase(thing)) {
      thing = null;
    }
  }

  /** Accessor for the qualify flag */
  public boolean isQualify() {
    return qualify;
  }

  /** Setter for the qualify flag */
  public void setQualify(boolean qualify) {
    this.qualify = qualify;
  }

  /** Accessor for the typed flag */
  public boolean isTyped() {
    return typed;
  }

  /** Setter for the typed flag */
  public void setTyped(boolean typed) {
    this.typed = typed;
  }
}
