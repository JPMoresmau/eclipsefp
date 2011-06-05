package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a "data" declaration.
 * 
 * @author serras
 */

public class DataType extends Gadt {

	public DataType(String doc, String[] context, String name, String[] vars,
			String kind, Constructor[] cons) {
		this.setDoc(doc);
		this.setType(DeclarationType.DATA_TYPE);
		this.setInfo(context, name, vars, kind, cons);
	}
	
	public DataType(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.setType(DeclarationType.DATA_TYPE);
		this.setInfo(o);
	}
}
