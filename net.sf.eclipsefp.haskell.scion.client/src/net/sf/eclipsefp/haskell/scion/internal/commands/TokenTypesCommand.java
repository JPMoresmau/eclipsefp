package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.Note;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * List token types, using GHC lexer
 * @author JP Moresmau
 *
 */
public class TokenTypesCommand extends ScionCommand {
	private String contents;
	private IFile file;
	private List<TokenDef> tokens;
	private boolean literate;
	
	public TokenTypesCommand(IScionCommandRunner runner, IScionServer server, IFile file, String contents,
	    boolean literate) {
		super(runner, server, Job.INTERACTIVE);
		this.file=file;
		this.contents=contents;
		this.literate=literate;
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = super.getParams();
		params.put("contents", contents);
		params.put("literate", literate);
		return params;
	}

	
	@Override
	protected void doProcessResult(Object json) throws JSONException {
		if (json instanceof JSONObject){
			JSONObject o=(JSONObject)json;
			JSONArray result=o.optJSONArray("Right");
			if (result!=null){
				tokens=new ArrayList<TokenDef>(result.length());
				for (int i = 0; i < result.length(); ++i) {
					JSONArray arr=result.getJSONArray(i);
					tokens.add(new TokenDef(arr));
				}
			} else {
				JSONObject err=o.optJSONObject("Left");
				try {
					new Note(err).applyAsMarker(file);
				} catch (CoreException ce){
					ce.printStackTrace();
				}
			}
		}
	}

	@Override
	protected String getMethod() {
		return "token-types";
	}

	public List<TokenDef> getTokens() {
		return tokens;
	}
}
