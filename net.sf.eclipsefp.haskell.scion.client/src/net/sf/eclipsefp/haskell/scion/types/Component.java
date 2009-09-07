package net.sf.eclipsefp.haskell.scion.types;

import org.json.JSONException;
import org.json.JSONObject;

public class Component {
	public enum ComponentType {
		FILE,
		LIBRARY,
		EXECUTABLE 
	}

	private ComponentType type;
	private String name;
	
	public Component(ComponentType type, String name) {
		super();
		this.type = type;
		this.name = name;
	}
	
	public Component(JSONObject obj){
		for (ComponentType ct:ComponentType.values()){
			String s=obj.optString(ct.name().toLowerCase());
			if (s.length()>0){
				type=ct;
				name=s;
				break;
			}
		}
		
	}

	public ComponentType getType() {
		return type;
	}

	public void setType(ComponentType type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public JSONObject toJSON() throws JSONException {
		JSONObject component = new JSONObject();
		component.put(getType().toString().toLowerCase(), getName());
		return component;
	}
	
	
	
}
