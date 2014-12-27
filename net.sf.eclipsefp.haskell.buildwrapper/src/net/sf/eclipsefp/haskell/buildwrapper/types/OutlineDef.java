package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;

import org.eclipse.core.resources.IFile;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Structure sent back from outline
 * @author JP Moresmau
 *
 */
public class OutlineDef {
	public enum OutlineDefType {
		CLASS,
		DATA,
		FAMILY,
		FUNCTION,
		PATTERN,
		SYN,
		TYPE,
		INSTANCE,
		FIELD,
		CONSTRUCTOR,
		SPLICE,
		ERROR
	}

	public static OutlineDefType parseType(JSONObject obj){
		String sType=obj.optString("type");
		if (sType!=null){
			try {
				return OutlineDefType.valueOf(sType.toUpperCase());
			} catch (IllegalArgumentException iae){
				BuildWrapperPlugin.logWarning(sType+" is not a valid outlinedef type", iae);
			}
		}
		return OutlineDefType.TYPE;
	}
	
	public static Comparator<OutlineDef> BY_LOCATION=new Comparator<OutlineDef>() {
		@Override
		public int compare(OutlineDef o1, OutlineDef o2) {
			Location l1=o1.getLocation();
			Location l2=o2.getLocation();
			if (l1.getStartLine()==l2.getStartLine()){
				return l1.getStartColumn()-l2.getStartColumn();
			} else {
				return l1.getStartLine()-l2.getStartLine();
			}
		}
	};
	
	public static Comparator<OutlineDef> BY_NAME=new Comparator<OutlineDef>() {
		@Override
		public int compare(OutlineDef o1, OutlineDef o2) {
			return o1.getName().compareToIgnoreCase(o2.getName());
		}
	};
	
	private Set<OutlineDefType> types;
	private String name;
	private Location loc;
	
	private List<OutlineDef> children=new ArrayList<>();
	
	/**
	 * the type signature or "?" if not known
	 */
	private String typeSignature;
	
	/**
	 * the comment or "" if none
	 */
	private String comment;
	/**
	 * the start line of the comment if any
	 */
	private Integer commentStartLine;
	
	//Location block
	public OutlineDef(String name, OutlineDefType type, Location loc) {
		super();
		this.name = name;
		types=new HashSet<>(1);
		this.types.add(type);
		this.loc = loc;
	}
	
	public OutlineDef(IFile f,JSONObject obj) throws JSONException{
		this.name=obj.getString("n");
		JSONArray arr=obj.getJSONArray("t");
		types=new HashSet<>(arr.length());
		for (int a=0;a<arr.length();a++){
			types.add(OutlineDefType.valueOf(arr.getString(a).toUpperCase(Locale.ENGLISH)));
		}
		this.loc=new Location(f,obj.getJSONArray("l"));
		arr=obj.getJSONArray("c");
		for (int a=0;a<arr.length();a++){
			children.add(new OutlineDef(f,arr.getJSONObject(a)));
		}
		typeSignature=obj.optString("s");
		if (typeSignature==null || typeSignature.length()==0 || "null".equals(typeSignature)){
			typeSignature="?";
		}
		comment=obj.optString("d");
		if (comment==null || "null".equals(comment)){
			comment="";
		}
		if (obj.has("sl") && !obj.isNull("sl")){
			commentStartLine=obj.getInt("sl");
		}
	}

	public List<OutlineDef> getChildren() {
		return children;
	}
	
	public String getID(){
		return types.toString()+":"+name;
	}
	
	public Set<OutlineDefType> getTypes() {
		return types;
	}

	public String getName() {
		return name;
	}

	public Location getLocation() {
		return loc;
	}

	
	@Override
	public String toString() {
		return getName();
	}

	public String getTypeSignature() {
		return typeSignature;
	}

	public String getComment() {
		return comment;
	}

	public Integer getCommentStartLine() {
		return commentStartLine;
	}
	
}
