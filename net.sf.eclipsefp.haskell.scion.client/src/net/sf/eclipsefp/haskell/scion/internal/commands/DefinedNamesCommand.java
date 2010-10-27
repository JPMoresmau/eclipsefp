package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;

public class DefinedNamesCommand extends ScionCommand {
	private List<String> names=new ArrayList<String>();
	
	public DefinedNamesCommand() {
		super();
	}

	@Override
  public String getMethod() {
		return "defined-names";
	}

	@Override
	protected void doProcessResult() throws JSONException {
		JSONArray arr = (JSONArray) response;
		for (int a=0;a<arr.length();a++){
			names.add(arr.getString(a));
		}
	}
	
	public List<String> getNames() {
		return names;
	}
}
