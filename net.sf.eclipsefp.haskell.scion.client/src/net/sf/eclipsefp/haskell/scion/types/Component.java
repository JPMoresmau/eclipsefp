package net.sf.eclipsefp.haskell.scion.types;

import org.json.JSONException;
import org.json.JSONObject;

public class Component {
	public enum ComponentType {
		FILE,
		LIBRARY,
		EXECUTABLE ,
		TESTSUITE
	}

	private ComponentType type;
	private String name;
	private String cabalFileName;
	private boolean buildable=true;
	
	public Component(ComponentType type, String name,String cabalFileName,boolean buildable) {
		super();
		this.type = type;
		this.name = name;
		this.cabalFileName=cabalFileName;
		this.buildable=buildable;
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
		if (obj.has("buildable")){
			buildable=obj.getBoolean("buildable");
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
	
	public boolean isBuildable() {
		return buildable;
	}
	
	public JSONObject toJSON() throws JSONException {
		JSONObject component = new JSONObject();
		// non cabal based
		if (ComponentType.FILE.equals(getType())){
			component.put("file", cabalFileName);
		} else {
			// cabal based
			component.put(getType().toString().toLowerCase(), getName()!=null?getName():JSONObject.NULL);
			component.put("cabal-file", cabalFileName);
			component.put("buildable", buildable);
		}
		return component;
	}
	
	@Override
	public String toString() {
		// this is equivalent to PackageDescriptionStanza getTypeName()
		if (ComponentType.TESTSUITE.equals(getType())){
			return "test-suite" +(getName()!=null?" "+getName():"");
		} else {
			return getType().toString().toLowerCase() +(getName()!=null?" "+getName():"");
		}
	}
	
}
