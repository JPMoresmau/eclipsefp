/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a Hoogle warning
 * @author JP Moresmau
 *
 */
public class HoogleResultWarning  extends HoogleResult {
	String warning;

	public HoogleResultWarning(String warning) {
		setType(HoogleResultType.WARNING);
		this.warning = warning;
	}
	
	public HoogleResultWarning(JSONObject o) throws JSONException {
		setType(HoogleResultType.WARNING);
		this.warning = o.getString("name");
	}

	@Override
	public String getName() {
		return warning;
	}

	@Override
	public String getCompleteDefinition() {
		return "Warning "+warning;
	}


}
