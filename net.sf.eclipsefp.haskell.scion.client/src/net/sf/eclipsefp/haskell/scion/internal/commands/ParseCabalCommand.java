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
	protected void doProcessResult() throws JSONException {
		description=(JSONObject) response; 
	}
	
	public JSONObject getDescription() {
		return description;
	}

	@Override
        public String getMethod() {
		return "parse-cabal";
	}
}
