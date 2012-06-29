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
 * Stylish Haskell import management options
 * @author JP Moresmau
 *
 */
public class SHImports {
	public static enum SHImportAlign {
		GLOBAL,
		GROUP,
		NONE;
	}
	
	private SHImportAlign align=SHImportAlign.GLOBAL;

	public SHImportAlign getAlign() {
		return align;
	}

	public void setAlign(SHImportAlign align) {
		if (align==null){
			align=SHImportAlign.GLOBAL;
		}
		this.align = align;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((align == null) ? 0 : align.hashCode());
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
		SHImports other = (SHImports) obj;
		if (align != other.align)
			return false;
		return true;
	}
	
	public void fromYAML(Map<?,?> map){
		Object o=map.get("align");
		if (o!=null){
			try {
				align=SHImportAlign.valueOf(String.valueOf(o).toUpperCase());	
			} catch (IllegalArgumentException iae){
				StylePlugin.logError(iae);
			}
		}
	}
	
	public Map<String,String> toYAML(){
		Map<String,String> m=new HashMap<String, String>();
		m.put("align", align.name().toLowerCase());
		return m;
	}
}
