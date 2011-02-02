package net.sf.eclipsefp.haskell.scion.internal.commands;

import org.json.JSONException;
import org.json.JSONObject;

public class CompletionTyCons extends ScionCommand {
  public CompletionTyCons() {
  }

  @Override
  protected void doProcessResult() throws JSONException {
    if (response instanceof JSONObject) {
      // Do something clever...
    }
  }
  
  @Override
  public String getMethod() {
    return "completion-tycons";
  }
}
