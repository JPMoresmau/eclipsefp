/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.json.JSONObject;

/**
 * The result of an evaluation in GHC
 * @author JP Moresmau
 *
 */
public class EvalResult {
	/**
	 * the type of the result, if any
	 */
	private String type;
	/**
	 * the output of the result, if any
	 */
	private String result;
	/**
	 * the error message, if any
	 */
	private String error;
	
	/**
	 * 
	 */
	public EvalResult() {

	}
	
	public EvalResult(JSONObject obj) {
		if (!obj.isNull("t")){
			type=obj.optString("t");
		}
		if (!obj.isNull("r")){
			result=obj.optString("r");
		}
		if (!obj.isNull("e")){
			error=obj.optString("e");
		}
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getResult() {
		return result;
	}

	public void setResult(String result) {
		this.result = result;
	}

	public String getError() {
		return error;
	}

	public void setError(String error) {
		this.error = error;
	}

}
