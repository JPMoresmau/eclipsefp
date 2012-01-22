/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
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

	/**
	 * was the build ok (no error in notes)
	 */
	private boolean buildOK=true;
	
	public OutlineResult() {
		super();

	}

	/**
	 * is there nothing in that result
	 * @return true if no declarations, no import and no export are contained in the result
	 */
	public boolean isEmpty(){
		return outlineDefs.isEmpty() && exportDefs.isEmpty() && importDefs.isEmpty();
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

	public boolean isBuildOK() {
		return buildOK;
	}

	public void setBuildOK(boolean buildOK) {
		this.buildOK = buildOK;
	}
	
	
}
