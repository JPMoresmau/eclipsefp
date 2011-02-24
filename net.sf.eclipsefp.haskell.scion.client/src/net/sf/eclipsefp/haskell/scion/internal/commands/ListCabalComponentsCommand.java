package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.types.Component;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ListCabalComponentsCommand extends ScionCommand {
	private String fileName;
	private List<Component> components=new LinkedList<Component>();
	
	public ListCabalComponentsCommand(String fileName) {
		super();
		this.fileName=fileName;
	}

	@Override
	protected void doProcessResult() throws JSONException {
		components.clear();
		if (response instanceof JSONObject){
			JSONObject o = (JSONObject) response;
		      JSONArray arr = o.optJSONArray("Right");
		      if (arr!=null){
			
				for (int a=0;a<arr.length();a++){
					components.add(new Component(arr.getJSONObject(a)));
				}
		      } else {
		    	  String msg=o.optString("Left");
		    	  throw new JSONException(msg);
		      }
		
		}

	}
	
	public List<Component> getComponents() {
		return components;
	}
	
	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("cabal-file", fileName);
		return params;
	}

	@Override
  public String getMethod() {
		return "list-cabal-components";
	}

}
