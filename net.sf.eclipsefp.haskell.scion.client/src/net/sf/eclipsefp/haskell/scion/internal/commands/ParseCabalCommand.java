package net.sf.eclipsefp.haskell.scion.internal.commands;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * 
 * @author JP Moresmau
 *
 */
public class ParseCabalCommand extends ScionCommand {
	private String fileName;
	private JSONObject description;
	
	public ParseCabalCommand(String fileName) {
		super();
		this.fileName=fileName;
	}
	
	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("cabal-file", fileName);
		return params;
	}
	
	@Override
	protected void doProcessResult(Object result) throws JSONException {
		description=(JSONObject)result; 
	}
	
	public JSONObject getDescription() {
		return description;
	}

	@Override
	protected String getMethod() {
		return "parse-cabal";
	}

}
