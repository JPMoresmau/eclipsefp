/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

import java.io.Serializable;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents all the information of an HLint suggestion.
 * @author Alejandro Serrano
 *
 */
public class Suggestion implements Serializable {
	private SourceLocation location;
	private Severity severity;
	private String message;
	private CodeModification pre;
	private CodeModification post;
	
	public SourceLocation getLocation() {
		return location;
	}
	
	public Severity getSeverity() {
		return severity;
	}
	
	public String getMessage() {
		return message;
	}
	
	public CodeModification getPre() {
		return pre;
	}
	
	public CodeModification getPost() {
		return post;
	}

	public void setLocation(SourceLocation location) {
		this.location = location;
	}

	public void setSeverity(Severity severity) {
		this.severity = severity;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public void setPre(CodeModification pre) {
		this.pre = pre;
	}

	public void setPost(CodeModification post) {
		this.post = post;
	}
	
	public String toString() {
		JSONObject obj=new JSONObject();
		try {
			if (pre!=null){
				obj.put("pre", pre.toJSON());
			}
			if (post!=null){
				obj.put("post", post.toJSON());
			}
			if (location!=null){
				obj.put("line", location.getLine());
				obj.put("column", location.getColumn());
			}
		} catch (JSONException je){
			je.printStackTrace();
		}
		return obj.toString();
	}
	
	public void fromString(String s){
		try {
			JSONObject obj=new JSONObject(s);
			pre=CodeModification.fromJSON(obj.opt("pre"));
			post=CodeModification.fromJSON(obj.opt("post"));
			int line=obj.optInt("line", -1);
			int column=obj.optInt("column",-1);
			if (line>-1 && column>-1){
				location=new SourceLocation("", line, column);
			}
		} catch (JSONException je){
			je.printStackTrace();
		}
	}
}
