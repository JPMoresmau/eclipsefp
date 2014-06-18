package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.ArrayList;
import java.util.List;


import org.eclipse.core.resources.IFile;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Export definition
 * @author JP Moresmau
 *
 */
public class ExportDef {
	private String name;
	private Location loc;
	private ImportExportType type;
	private List<String> children=new ArrayList<>();
	
	
	public ExportDef(String name, Location loc, ImportExportType type) {
		super();
		this.name = name;
		this.loc = loc;
		this.type = type;
	}
	
	public ExportDef(IFile f,JSONObject obj) throws JSONException{
		this.name=obj.getString("n");
		type=ImportExportType.valueOf(obj.getString("t"));
		this.loc=new Location(f,obj.getJSONArray("l"));
		JSONArray arr=obj.getJSONArray("c");
		for (int a=0;a<arr.length();a++){
			children.add(arr.getString(a));
		}
	}
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public Location getLocation() {
		return loc;
	}
	public void setLocation(Location loc) {
		this.loc = loc;
	}
	public ImportExportType getType() {
		return type;
	}
	public void setType(ImportExportType type) {
		this.type = type;
	}
	public List<String> getChildren() {
		return children;
	}
	
	
}
