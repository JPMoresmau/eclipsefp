/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell;

import java.util.HashMap;
import java.util.Map;

/**
 * Stylish Haskell unicode management options
 * @author JP Moresmau
 *
 */
public class SHUnicode {
	private boolean unicodePragmas=false;

	public boolean isUnicodePragmas() {
		return unicodePragmas;
	}

	public void setUnicodePragmas(boolean unicodePragmas) {
		this.unicodePragmas = unicodePragmas;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (unicodePragmas ? 1231 : 1237);
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
		SHUnicode other = (SHUnicode) obj;
		if (unicodePragmas != other.unicodePragmas)
			return false;
		return true;
	}
	
	public void fromYAML(Map<?,?> map){
		Object o=map.get("add_language_pragma");
		if (o!=null){
			unicodePragmas=Boolean.TRUE.toString().equalsIgnoreCase(String.valueOf(o));
		}
	}
	
	public Map<String,Object> toYAML(){
		Map<String,Object> m=new HashMap<>();
		m.put("add_language_pragma", unicodePragmas);
		return m;
	}
}
