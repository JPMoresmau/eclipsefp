/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a GADT constructor.
 * 
 * @author Alejandro Serrano
 */
public class Constructor extends Documented {
	private String name;
	private String signature;
	private String typeName;
	
	
	public Constructor(String doc, String name, String signature,String typeName) {
		this.setDoc(doc);
		this.name = name;
		this.signature = signature;
		this.typeName = typeName;
	}
	
	public Constructor(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.name = o.getString("name");
		this.signature = o.getString("type");
	}
	
	@Override
	public String getName() {
		return this.name;
	}
	
	public String getSignature() {
		return this.signature;
	}
	
	@Override
	public String getCompleteDefinition() {
		StringBuilder builder = new StringBuilder(this.getName());
		builder.append(" :: ");
		builder.append(this.getSignature());
		return builder.toString();
	}
	
	public String getShownName() {
		return this.name + " :: " + this.signature;
	}

	public String getTypeName() {
		return typeName;
	}

	public void setTypeName(String typeName) {
		this.typeName = typeName;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.browser.items.Documented#isType()
	 */
	@Override
	public boolean isType() {
		return false;
	}
}
