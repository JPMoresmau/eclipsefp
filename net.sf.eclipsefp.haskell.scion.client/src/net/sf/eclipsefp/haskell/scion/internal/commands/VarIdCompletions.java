package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.json.JSONException;
import org.json.JSONObject;

public class VarIdCompletions extends ScionCommand {
  IFile theFile;
  Map<String, String> completions;
  
  public VarIdCompletions(final IFile theFile) {
    this.theFile = theFile;
    this.completions = null;
  }

  @Override
  protected JSONObject getParams() throws JSONException {
    JSONObject params = new JSONObject();
    params.put("file", theFile.getLocation().toOSString());
    return params;
  }

  @Override
  protected void doProcessResult() throws JSONException {
    if (response instanceof JSONObject) {
      // Convert from JSON's representation to a simple map of strings
      JSONObject obj = (JSONObject) response;
      completions = new HashMap<String, String>();
      Iterator<String> ki = obj.keys();
      while (ki.hasNext()) {
        String k = ki.next();
        completions.put(k, obj.getString(k));
      }
    }
  }
  
  @Override
  public String getMethod() {
    return "completion-types";
  }
  
  public final Map<String, String> getCompletions() {
    return completions;
  }
}
