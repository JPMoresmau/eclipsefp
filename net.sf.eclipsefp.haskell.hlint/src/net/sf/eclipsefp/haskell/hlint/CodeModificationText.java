/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a suggestion of exchanging some text
 * with another text.
 * @author Alejandro Serrano
 *
 */
public class CodeModificationText extends CodeModification {
	private String text;
	
	public CodeModificationText(String text) {
		setType(CodeModificationType.TEXT);
		this.text = text;
	}
	
	public String getText() {
		return this.text;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.hlint.CodeModification#toJSON()
	 */
	@Override
	public Object toJSON() throws JSONException{
		JSONObject obj=new JSONObject();
		obj.put(CodeModificationType.TEXT.toString(), this.text);
		return obj;
	}
}
