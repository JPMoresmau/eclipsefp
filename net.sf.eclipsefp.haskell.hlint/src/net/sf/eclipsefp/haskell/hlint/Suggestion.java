/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

import java.io.Serializable;

import net.sf.eclipsefp.haskell.hlint.util.HLintText;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents all the information of an HLint suggestion.
 * @author Alejandro Serrano
 *
 */
public class Suggestion implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1068207338373141685L;
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
	
	public String getPreText(){
		if (pre instanceof CodeModificationText){
			return ((CodeModificationText)pre).getText();
		}
		return null;
	}
	
	public String getPostText(){
		if (post instanceof CodeModificationText){
			return ((CodeModificationText)post).getText();
		}
		return null;
	}
	
	/**
	 * get the text suitable for the marker description
	 * @param alwaysFull always show the suggestions even if we can apply them automatically?
	 * @return
	 */
	public String getMarkerText(boolean alwaysFull){
		// we can't fix automatically, let's give as much info as we can
		if (alwaysFull || !HLintFixer.canFix(this)){
			String pre=getPreText();
			String post=getPostText();
			if (pre!=null && post!=null){
				String nl=" "+PlatformUtil.NL; // leave space for problems view visibility
				StringBuilder sb=new StringBuilder();
				sb.append(getMessage());
				sb.append(nl);
				sb.append(HLintText.suggestion_found);
				sb.append(nl);
				sb.append(pre);
				sb.append(nl);
				sb.append(HLintText.suggestion_post);
				sb.append(nl);
				sb.append(post);
				return sb.toString();
			}
		}
		return getMessage();
	}
	
	@Override
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
