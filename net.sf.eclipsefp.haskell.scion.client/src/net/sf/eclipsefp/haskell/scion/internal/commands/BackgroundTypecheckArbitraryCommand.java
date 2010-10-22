package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionInstance;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * 
 * @author JP Moresmau
 *
 */
public class BackgroundTypecheckArbitraryCommand extends BackgroundTypecheckFileCommand {
	private IDocument doc;
	
	public BackgroundTypecheckArbitraryCommand(ScionInstance instance, IFile file, IDocument doc) {
		super(instance, file);
		this.doc=doc;
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = super.getParams();
		params.put("contents", doc.get());
		return params;
	}

	@Override
	protected void doProcessCompilationResult(){
		new CompilationResultHandler(file.getProject(),doc,file.getLocation()).process(this);
	}
	
  @Override
  protected String getMethod() {
    return "background-typecheck-arbitrary";
  }

}
