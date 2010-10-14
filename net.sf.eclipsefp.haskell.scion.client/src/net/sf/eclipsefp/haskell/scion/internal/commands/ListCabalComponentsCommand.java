package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.Component;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ListCabalComponentsCommand extends ScionCommand {
	private String fileName;
	private List<Component> components=new LinkedList<Component>();
	
	public ListCabalComponentsCommand(IScionCommandRunner runner, IScionServer server, int priority,
	    String fileName) {
		super(runner, server, priority);
		this.fileName=fileName;
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		components.clear();
		if (result instanceof JSONArray){
			JSONArray arr=(JSONArray)result;
			for (int a=0;a<arr.length();a++){
				components.add(new Component(arr.getJSONObject(a)));
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
	protected String getMethod() {
		return "list-cabal-components";
	}

}
