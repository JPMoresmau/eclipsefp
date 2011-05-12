package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.types.Note;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
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
	
	public TokenTypesCommand(IFile file, String contents, boolean literate) {
		super();
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
	protected void doProcessResult() throws JSONException {
		if (response instanceof JSONObject){
			JSONObject o=(JSONObject) response;
			JSONArray result=o.optJSONArray("Right");
			if (result!=null){
				tokens=new ArrayList<TokenDef>(result.length());
//				TokenDef previous=null;
				for (int i = 0; i < result.length(); ++i) {
					JSONArray arr=result.getJSONArray(i);
					TokenDef td=new TokenDef(arr);
					// checks
//					if (td.getLocation().getStartLine()>td.getLocation().getEndLine()){
//						System.err.println(td.getLocation().toString());
//					}
//					if (td.getLocation().getStartLine()==td.getLocation().getEndLine() && td.getLocation().getStartColumn()>=td.getLocation().getEndColumn()){
//						System.err.println(td.getLocation().toString());
//					}
//					if (previous!=null){
//						if (td.getLocation().getStartLine()<previous.getLocation().getStartLine()){
//							System.err.println(td.getLocation().toString());
//						}
//						if (td.getLocation().getStartLine()<previous.getLocation().getEndLine()){
//							System.err.println(td.getLocation().toString());
//						}
//						if (td.getLocation().getStartLine()==previous.getLocation().getEndLine()){
//							if (td.getLocation().getStartColumn()<previous.getLocation().getEndColumn()){
//								System.err.println(td.getLocation().toString());
//							}
//						}
//					}
//					previous=td;
					tokens.add(td);
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
  public String getMethod() {
		return "token-types";
	}

	public List<TokenDef> getTokens() {
		return tokens;
	}
}
