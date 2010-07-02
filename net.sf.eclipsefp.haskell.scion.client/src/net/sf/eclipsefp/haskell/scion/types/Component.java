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
	private String cabalFileName;
	
	public Component(ComponentType type, String name,String cabalFileName) {
		super();
		this.type = type;
		this.name = name;
		this.cabalFileName=cabalFileName;
	}
	
	public Component(JSONObject obj) throws JSONException{
		for (ComponentType ct:ComponentType.values()){
			Object o=obj.opt(ct.name().toLowerCase());
			if (o!=null){
				type=ct;
				if (o instanceof String){
					name=(String)o;
				}
				break;
			}
			/*String s=obj.optString(ct.name().toLowerCase());
			if (s.length()>0){
				type=ct;
				name=s;
				break;
			}*/
		}
		cabalFileName=obj.getString("cabal-file");
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
		component.put(getType().toString().toLowerCase(), getName()!=null?getName():JSONObject.NULL);
		component.put("cabal-file", cabalFileName);
		return component;
	}
	
	@Override
	public String toString() {
		return getType().toString().toLowerCase() +" "+(getName()!=null?getName():"");
	}
	
}
