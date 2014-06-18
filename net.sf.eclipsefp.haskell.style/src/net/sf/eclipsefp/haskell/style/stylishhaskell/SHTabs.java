/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell;

import java.util.HashMap;
import java.util.Map;

import net.sf.eclipsefp.haskell.style.StylePlugin;

/**
 * Stylish Haskell tabs management options
 * @author JP Moresmau
 *
 */
public class SHTabs {
	private int spaces=8;

	public int getSpaces() {
		return spaces;
	}

	public void setSpaces(int spaces) {
		this.spaces = spaces;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + spaces;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		SHTabs other = (SHTabs) obj;
		if (spaces != other.spaces)
			return false;
		return true;
	}
	
	public void fromYAML(Map<?,?> map){
		Object o=map.get("spaces");
		if (o!=null){
			try {
				spaces=Integer.parseInt(String.valueOf(o));	
			} catch (NumberFormatException nfe){
				StylePlugin.logError(nfe);
			}
		}
	}
	
	public Map<String,Object> toYAML(){
		Map<String,Object> m=new HashMap<>();
		m.put("spaces", spaces);
		return m;
	}
}
