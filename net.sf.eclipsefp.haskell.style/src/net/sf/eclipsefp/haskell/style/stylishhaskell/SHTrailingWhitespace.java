/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell;

import java.util.HashMap;
import java.util.Map;

/**
 * Stylish Haskell trailing space management options
 * @author JP Moresmau
 *
 */
public class SHTrailingWhitespace {

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return 0;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return true;
	}
	
	public void fromYAML(Map<?,?> map){
		
	}
	
	public Map<String,String> toYAML(){
		Map<String,String> m=new HashMap<String, String>();
		return m;
	}
}
