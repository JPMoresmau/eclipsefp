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
 * Stylish Haskell language pragmas management options
 * @author JP Moresmau
 *
 */
public class SHPragmas {
	public static enum SHPragmaStyle{
		VERTICAL,
		COMPACT;
	}
	
	private SHPragmaStyle style=SHPragmaStyle.VERTICAL;

	private boolean removeRedundant=true;
	
	public SHPragmaStyle getStyle() {
		return style;
	}

	public void setStyle(SHPragmaStyle style) {
		this.style = style;
	}

	public boolean isRemoveRedundant() {
		return removeRedundant;
	}

	public void setRemoveRedundant(boolean removeRedundant) {
		this.removeRedundant = removeRedundant;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (removeRedundant ? 1231 : 1237);
		result = prime * result + ((style == null) ? 0 : style.hashCode());
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
		SHPragmas other = (SHPragmas) obj;
		if (removeRedundant != other.removeRedundant)
			return false;
		if (style != other.style)
			return false;
		return true;
	}
	
	public void fromYAML(Map<?,?> map){
		Object o=map.get("style");
		if (o!=null){
			try {
				style=SHPragmaStyle.valueOf(String.valueOf(o).toUpperCase());	
			} catch (IllegalArgumentException iae){
				StylePlugin.logError(iae);
			}
		}
		o=map.get("remove_redundant");
		if (o!=null){
			removeRedundant=Boolean.TRUE.toString().equalsIgnoreCase(String.valueOf(o));
		}
	}
	
	public Map<String,Object> toYAML(){
		Map<String,Object> m=new HashMap<String, Object>();
		m.put("style", style.name().toLowerCase());
		m.put("remove_redundant", removeRedundant);
		return m;
	}
}
