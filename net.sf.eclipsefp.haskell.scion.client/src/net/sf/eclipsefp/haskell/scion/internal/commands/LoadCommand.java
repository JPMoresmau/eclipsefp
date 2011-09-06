package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.scion.types.BuildOptions;
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
  private BuildOptions		buildOptions;
  private IProject          project;
  
  public static final String ID_SCION_MARKER = "net.sf.eclipsefp.haskell.core.scionProblem"; //$NON-NLS-1$

  public LoadCommand(IProject project, Component c, BuildOptions options) {
    super();
    if (options==null){
    	throw new IllegalArgumentException("buildoptions == null");
    }
    this.comp = c;
    this.buildOptions=options;
    this.project = project;
  }

  @Override
  public String getMethod() {
    return "load";
  }

  @Override
  protected JSONObject getParams() throws JSONException {
    JSONObject params = new JSONObject();
    params.put("component", comp.toJSON());
    JSONObject options = new JSONObject();
    options.put("output", buildOptions.isOutput());
    options.put("forcerecomp", buildOptions.isRecompile());
    params.put("options", options);
    return params;
  }

  @Override
  protected void doProcessResult() throws JSONException {
    compilationResult = new CompilationResult((JSONObject) response);
  }

  public CompilationResult getCompilationResult() {
    return compilationResult;
  }

  public boolean hasOutput() {
    return buildOptions.isOutput();
  }

  @Override
  public boolean onError(String name, String message) {
    try {
      IMarker marker = project.createMarker(ID_SCION_MARKER);
      marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
      marker.setAttribute(IMarker.MESSAGE, message);
    } catch (CoreException ce) {
      ScionPlugin.logError(ScionText.error_applyMarkers, ce);
      ce.printStackTrace();
    }
    return true;
  }
}
