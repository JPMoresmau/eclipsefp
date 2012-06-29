/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A stylish haskell configuration
 * @author JP Moresmau
 *
 */
public class SHConfiguration {
	private SHUnicode unicode=null;
	
	private SHImports imports=new SHImports();
	
	private SHPragmas pragmas=new SHPragmas();
	
	private SHTabs tabs=null;
	
	private SHTrailingWhitespace trailingWhitespace=new SHTrailingWhitespace();

	public SHUnicode getUnicode() {
		return unicode;
	}

	public void setUnicode(SHUnicode unicode) {
		this.unicode = unicode;
	}

	public SHImports getImports() {
		return imports;
	}

	public void setImports(SHImports imports) {
		this.imports = imports;
	}

	public SHPragmas getPragmas() {
		return pragmas;
	}

	public void setPragmas(SHPragmas pragmas) {
		this.pragmas = pragmas;
	}

	public SHTabs getTabs() {
		return tabs;
	}

	public void setTabs(SHTabs tabs) {
		this.tabs = tabs;
	}

	public SHTrailingWhitespace getTrailingWhitespace() {
		return trailingWhitespace;
	}

	public void setTrailingWhitespace(SHTrailingWhitespace trailingWhitespace) {
		this.trailingWhitespace = trailingWhitespace;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((imports == null) ? 0 : imports.hashCode());
		result = prime * result + ((pragmas == null) ? 0 : pragmas.hashCode());
		result = prime * result + ((tabs == null) ? 0 : tabs.hashCode());
		result = prime
				* result
				+ ((trailingWhitespace == null) ? 0 : trailingWhitespace
						.hashCode());
		result = prime * result + ((unicode == null) ? 0 : unicode.hashCode());
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
		SHConfiguration other = (SHConfiguration) obj;
		if (imports == null) {
			if (other.imports != null)
				return false;
		} else if (!imports.equals(other.imports))
			return false;
		if (pragmas == null) {
			if (other.pragmas != null)
				return false;
		} else if (!pragmas.equals(other.pragmas))
			return false;
		if (tabs == null) {
			if (other.tabs != null)
				return false;
		} else if (!tabs.equals(other.tabs))
			return false;
		if (trailingWhitespace == null) {
			if (other.trailingWhitespace != null)
				return false;
		} else if (!trailingWhitespace.equals(other.trailingWhitespace))
			return false;
		if (unicode == null) {
			if (other.unicode != null)
				return false;
		} else if (!unicode.equals(other.unicode))
			return false;
		return true;
	}
	
	public void fromYAML(Object o){
		clear();
		if (o instanceof Map<?, ?>){
			Object val1=((Map<?,?>)o).get("steps");
			if (val1 instanceof Collection<?>){
				for (Object o2:(Collection<?>)val1){
					if (o2 instanceof Map<?,?>){
						Map<?,?> m2=(Map<?,?>)o2;
						if (!m2.isEmpty()){
							Map.Entry<?, ?> e=m2.entrySet().iterator().next();
							Object k1=e.getKey();
							if (k1 instanceof String){
								Object v1=e.getValue();
								if (v1 instanceof Map<?,?>){
									addStep((String)k1,(Map<?,?>)v1);
								}
							}
						}
					}
				}
			}
		}
	}
	
	public Map<String,Object> toYAML(){
		List<Object> steps=new ArrayList<Object>();
		if (unicode!=null){
			Map<String,Object> m=new HashMap<String, Object>();
			m.put("unicode_syntax", unicode.toYAML());
			steps.add(m);
		}
		if (imports!=null){
			Map<String,Object> m=new HashMap<String, Object>();
			m.put("imports", imports.toYAML());
			steps.add(m);
		}
		if (pragmas!=null){
			Map<String,Object> m=new HashMap<String, Object>();
			m.put("language_pragmas", pragmas.toYAML());
			steps.add(m);
		}
		if (tabs!=null){
			Map<String,Object> m=new HashMap<String, Object>();
			m.put("tabs", tabs.toYAML());
			steps.add(m);
		}
		if (trailingWhitespace!=null){
			Map<String,Object> m=new HashMap<String, Object>();
			m.put("trailing_whitespace", trailingWhitespace.toYAML());
			steps.add(m);
		}
		
		Map<String,Object> ret=new HashMap<String, Object>();
		ret.put("steps", steps);
		return ret;
	}
	
	public void clear(){
		imports=null;
		pragmas=null;
		tabs=null;
		trailingWhitespace=null;
		unicode=null;
	}
	
	private void addStep(String name,Map<?,?> params){
		if ("unicode_syntax".equalsIgnoreCase(name)){
			unicode=new SHUnicode();
			unicode.fromYAML(params);
		} else if ("imports".equalsIgnoreCase(name)){
			imports=new SHImports();
			imports.fromYAML(params);
		} else if ("language_pragmas".equalsIgnoreCase(name)){
			pragmas=new SHPragmas();
			pragmas.fromYAML(params);
		} else if ("tabs".equalsIgnoreCase(name)){
			tabs=new SHTabs();
			tabs.fromYAML(params);
		}else if ("trailing_whitespace".equalsIgnoreCase(name)){
			trailingWhitespace=new SHTrailingWhitespace();
			trailingWhitespace.fromYAML(params);
		}
	}
}
