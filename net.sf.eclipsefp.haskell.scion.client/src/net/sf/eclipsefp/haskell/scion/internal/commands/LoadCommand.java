package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.scion.types.CompilationResult;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.ICompilerResult;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Command that loads a given component into the Scion server.
 * 
 * @author Thomas ten Cate
 */
public class LoadCommand extends ScionCommand implements ICompilerResult {
  private Component         comp;
  private CompilationResult compilationResult;
  private boolean           output;
  private boolean           forceRecomp;
  private IProject          project;

  public LoadCommand(IProject project, Component c, boolean output, boolean forceRecomp) {
    super();
    this.comp = c;
    this.output = output;
    this.forceRecomp = forceRecomp;
    this.project = project;
  }

  @Override
  protected String getMethod() {
    return "load";
  }

  @Override
  protected JSONObject getParams() throws JSONException {
    JSONObject params = new JSONObject();
    params.put("component", comp.toJSON());
    JSONObject options = new JSONObject();
    options.put("output", output);
    options.put("forcerecomp", forceRecomp);
    params.put("options", options);
    return params;
  }

  @Override
  protected void doProcessResult(Object result) throws JSONException {
    compilationResult = new CompilationResult((JSONObject) result);

  }

  public CompilationResult getCompilationResult() {
    return compilationResult;
  }

  public boolean hasOutput() {
    return output;
  }

  @Override
  public boolean onError(String name, String message) {
    try {
      IMarker marker = project.createMarker(IMarker.PROBLEM);
      marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
      marker.setAttribute(IMarker.MESSAGE, message);
    } catch (CoreException ce) {
      ScionPlugin.logError(ScionText.error_applyMarkers, ce);
      ce.printStackTrace();
    }
    return true;
  }
}
