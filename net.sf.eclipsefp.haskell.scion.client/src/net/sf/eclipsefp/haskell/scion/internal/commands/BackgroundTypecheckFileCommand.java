package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.types.CompilationResult;
import net.sf.eclipsefp.haskell.scion.types.ICompilerResult;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.Note;
import net.sf.eclipsefp.haskell.scion.types.Note.Kind;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class BackgroundTypecheckFileCommand extends ScionCommand implements ICompilerResult {
  protected IFile           file;
  private CompilationResult compilationResult;

  protected ScionInstance   instance;

  public BackgroundTypecheckFileCommand(ScionInstance instance, IFile file) {
    super();
    this.file = file;
    this.instance = instance;
  }

  @Override
  public String getMethod() {
    return "background-typecheck-file";
  }

  @Override
  protected JSONObject getParams() throws JSONException {
    JSONObject params = new JSONObject();
    params.put("file", file.getLocation().toOSString());
    return params;
  }

  @Override
  protected void doProcessResult() throws JSONException {
    instance.deleteProblems(file);
    if (response instanceof JSONArray) {
      JSONArray result = (JSONArray) response;
      compilationResult = new CompilationResult(result.getJSONObject(1));
    } else if (response instanceof JSONObject) {
      JSONObject o = (JSONObject) response;
      JSONObject cr = o.optJSONObject("Right");
      if (cr != null) {
        instance.setLoadedFile(file);
        compilationResult = new CompilationResult(cr);
      } else {
        String err = o.optString("Left");
        try {
          new Note(Kind.ERROR, new Location(file.getLocation().toOSString(), 1, 1, 1, 1), err, null).applyAsMarker(file);
        } catch (CoreException ce) {
          ce.printStackTrace();
        }
      }
    }
    doProcessCompilationResult();
  }

  protected void doProcessCompilationResult() {
    new CompilationResultHandler(file.getProject()).process(this);
  }

  @Override
  public boolean onError(String name, String message) {
    instance.deleteProblems(file);
    compilationResult = new CompilationResult(file.getLocation().toOSString(), message);
    doProcessCompilationResult();
    return true;
  }

  public CompilationResult getCompilationResult() {
    return compilationResult;
  }

  public boolean hasOutput() {
    return false;
  }

}
