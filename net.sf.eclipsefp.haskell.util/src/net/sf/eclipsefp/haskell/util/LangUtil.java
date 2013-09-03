/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import java.util.Collection;

/**
 * Small utility methods, usually to do with Strings or Collections
 * @author JP Moresmau
 *
 */
public class LangUtil {

	public static <T> String join (Collection<T> cols,String sep){
		if (cols==null || cols.size()==0){
			return "";
		}
		String s="";
		StringBuilder ret=new StringBuilder();
		for (T o:cols){
			ret.append(s);
			s=sep;
			ret.append(String.valueOf(o));
		}
		return ret.toString();
	}
	
	/**
	 * remove double quotes around a string
	 * @param s
	 * @return
	 */
	public static String unquote(String s){
		if (s!=null && s.length()>1){
			if (s.charAt(0)=='\"' && s.charAt(s.length()-1)=='\"'){
				s=s.substring(1,s.length()-1);
			}
		}
		return s;
	}
	
	/**
	 * trim spaces only on the left
	 * @param s
	 * @return
	 */
	public static String ltrim(String s) {
	    int i = 0;
	    while (i < s.length() && Character.isWhitespace(s.charAt(i))) {
	        i++;
	    }
	    return s.substring(i);
	}

	/**
	 * trim spaces only on the right
	 * @param s
	 * @return
	 */
	public static String rtrim(String s) {
	    int i = s.length()-1;
	    while (i >= 0 && Character.isWhitespace(s.charAt(i))) {
	        i--;
	    }
	    return s.substring(0,i+1);
	}

}
