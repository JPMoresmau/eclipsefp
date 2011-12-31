package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


/**
 * Result of Outline call
 * @author JP Moresmau
 *
 */
public class OutlineResult {
	private List<OutlineDef> outlineDefs=new ArrayList<OutlineDef>();
	private List<ExportDef> exportDefs=new ArrayList<ExportDef>();
	private List<ImportDef> importDefs=new ArrayList<ImportDef>();

	public OutlineResult() {
		super();

	}

	
	public OutlineResult(IFile f,JSONObject obj) throws JSONException{
		JSONArray arr=obj.getJSONArray("o");
		for (int a=0;a<arr.length();a++){
			outlineDefs.add(new OutlineDef(f,arr.getJSONObject(a)));
		}
		arr=obj.getJSONArray("e");
		for (int a=0;a<arr.length();a++){
			exportDefs.add(new ExportDef(f,arr.getJSONObject(a)));
		}
		arr=obj.getJSONArray("i");
		for (int a=0;a<arr.length();a++){
			importDefs.add(new ImportDef(f,arr.getJSONObject(a)));
		}
	}
	
	public List<OutlineDef> getOutlineDefs() {
		return outlineDefs;
	}

	public List<ExportDef> getExportDefs() {
		return exportDefs;
	}

	public List<ImportDef> getImportDefs() {
		return importDefs;
	}
	
	
}
