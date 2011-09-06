package org.json;

/**
 * Navigates several roles at once inside a JSON object hierarchy
 * @author JP Moresmau
 *
 */
public class JSONNavigator {

	public static Object opt(JSONObject obj,String ... path){
		return optInternal(obj,0,path);
	}
	
	public static Object opt(JSONArray arr,String ... path){
		return optInternal(arr,0,path);
	}	
	
	private static Object optInternal(JSONObject obj,int ix,String ... path){
		if (obj==null || path==null || ix>=path.length){
			return null;
		}
		Object res=obj.opt(path[ix]);
		return optInternal(res,ix,path);
		
	}
	
	private static Object optInternal(JSONArray arr,int ix,String ... path){
		if (arr==null || path==null || ix>=path.length){
			return null;
		}
		String s=String.valueOf(path[ix]);
		if ("length".equals(s)){
			return arr.length();
		}
		Object res=arr.opt(Integer.parseInt(s));
		return optInternal(res,ix,path);
	}
	
	private static Object optInternal(Object obj,int ix,String ... path){
		ix++;
		if (ix>=path.length){
			return obj;
		} else if (obj instanceof JSONObject){
			return optInternal((JSONObject)obj,ix,path);
		} else if (obj instanceof JSONArray){
			return optInternal((JSONArray)obj,ix,path);
		}
		return null;
	}
	
	
	
}
