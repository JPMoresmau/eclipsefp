/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import java.util.Collection;

/**
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
}
