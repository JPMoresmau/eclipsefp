/**
 * (c) 2012, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents the information from a keyword returned
 * by a Hoogle search.
 * 
 * @author Alejandro Serrano
 */
public class HoogleResultKeyword extends HoogleResult {
	String keyword;

	public HoogleResultKeyword(String keyword) {
		setType(HoogleResultType.KEYWORD);
		this.keyword = keyword;
	}
	
	public HoogleResultKeyword(JSONObject o) throws JSONException {
		setType(HoogleResultType.KEYWORD);
		this.keyword = o.getString("name");
	}
	
	public String getKeyword() {
		return this.keyword;
	}

	@Override
	public String getName() {
		return this.keyword;
	}
	
	@Override
	public String getCompleteDefinition() {
		return "keyword " + this.getName();
	}
}
