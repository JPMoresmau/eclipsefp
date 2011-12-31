package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * module import spec
 * @author JP Moresmau
 *
 */
public class ImportDef {
	private String module;
	private Location loc;
	private boolean qualified;
	private boolean hiding;
	private String alias;
	
	private List<ImportSpecDef> children=null;

	
	
	public ImportDef(String module, Location loc, boolean qualified,
			boolean hiding, String alias) {
		super();
		this.module = module;
		this.loc = loc;
		this.qualified = qualified;
		this.hiding = hiding;
		this.alias = alias;
	}

	public ImportDef(IFile f,JSONObject obj) throws JSONException{
		module=obj.getString("m");
		this.loc=new Location(f,obj.getJSONArray("l"));
		qualified=obj.optBoolean("q", false);
		hiding=obj.optBoolean("h", false);
		alias=obj.optString("a");
		JSONArray arr=obj.optJSONArray("c");
		if (arr!=null){
			children=new ArrayList<ImportSpecDef>();
			for (int a=0;a<arr.length();a++){
				children.add(new ImportSpecDef(f,arr.getJSONObject(a)));
			}
		}
	}
	
	public String getModule() {
		return module;
	}

	public void setModule(String module) {
		this.module = module;
	}

	public Location getLocation() {
		return loc;
	}

	public void setLocation(Location loc) {
		this.loc = loc;
	}

	public boolean isQualified() {
		return qualified;
	}

	public void setQualified(boolean qualified) {
		this.qualified = qualified;
	}

	public boolean isHiding() {
		return hiding;
	}

	public void setHiding(boolean hiding) {
		this.hiding = hiding;
	}

	public String getAlias() {
		return alias;
	}

	public void setAlias(String alias) {
		this.alias = alias;
	}

	public List<ImportSpecDef> getChildren() {
		return children;
	}

	public void setChildren(List<ImportSpecDef> children) {
		this.children = children;
	}
	
	
	
}
