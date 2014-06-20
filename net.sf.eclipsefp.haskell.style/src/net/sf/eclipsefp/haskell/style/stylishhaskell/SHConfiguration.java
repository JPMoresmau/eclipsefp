/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.style.StylePlugin;

/**
 * A stylish haskell configuration
 * @author JP Moresmau
 *
 */
public class SHConfiguration implements Cloneable {
	private SHUnicode unicode=null;
	
	private SHImports imports=new SHImports();
	
	private SHPragmas pragmas=new SHPragmas();
	
	private SHTabs tabs=null;
	
	private SHTrailingWhitespace trailingWhitespace=new SHTrailingWhitespace();

	private SHRecords records = new SHRecords();
	
	public static final int DEFAULT_COLUMNS=80;
	
	/**
	 * number of columns to adapt formatting to
	 */
	private int columns=DEFAULT_COLUMNS;
	
	/**
	 * the extensions needed to parse the file correctly
	 */
	private Set<String> languageExtensions = new HashSet<>();
	
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
		result = prime * result + columns;
		result = prime * result + ((imports == null) ? 0 : imports.hashCode());
		result = prime
				* result
				+ ((languageExtensions == null) ? 0 : languageExtensions
						.hashCode());
		result = prime * result + ((pragmas == null) ? 0 : pragmas.hashCode());
		result = prime * result + ((records == null) ? 0 : records.hashCode());
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
		if (columns != other.columns)
			return false;
		if (imports == null) {
			if (other.imports != null)
				return false;
		} else if (!imports.equals(other.imports))
			return false;
		if (languageExtensions == null) {
			if (other.languageExtensions != null)
				return false;
		} else if (!languageExtensions.equals(other.languageExtensions))
			return false;
		if (pragmas == null) {
			if (other.pragmas != null)
				return false;
		} else if (!pragmas.equals(other.pragmas))
			return false;
		if (records == null) {
			if (other.records != null)
				return false;
		} else if (!records.equals(other.records))
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
			Map<?,?> m=(Map<?,?>)o;
			Object val1=m.get("steps");
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
			columns=80;
			Object val2=m.get("columns");
			if (val2!=null){
				if (val2 instanceof Integer){
					columns=((Integer)val2).intValue();
				} else {
					try {
						columns=Integer.parseInt(String.valueOf(val2));	
					} catch (NumberFormatException nfe){
						StylePlugin.logError(nfe);
					}
				}
			}
			Object val3=m.get("language_extensions");
			if (val3 instanceof String[]){
				for (String ext:(String[])val3){
					languageExtensions.add(ext);
				}
			} else if (val3 instanceof Collection<?>){
				for (Object ext:(Collection<?>)val3){
					languageExtensions.add(String.valueOf(ext));
				}
			}
		}
	}
	
	public Map<String,Object> toYAML(){
		List<Object> steps=new ArrayList<>();
		if (unicode!=null){
			Map<String,Object> m=new HashMap<>();
			m.put("unicode_syntax", unicode.toYAML());
			steps.add(m);
		}
		if (imports!=null){
			Map<String,Object> m=new HashMap<>();
			m.put("imports", imports.toYAML());
			steps.add(m);
		}
		if (pragmas!=null){
			Map<String,Object> m=new HashMap<>();
			m.put("language_pragmas", pragmas.toYAML());
			steps.add(m);
		}
		if (records!=null){
			Map<String,Object> m=new HashMap<>();
			m.put("records", records.toYAML());
			steps.add(m);
		}
		if (tabs!=null){
			Map<String,Object> m=new HashMap<>();
			m.put("tabs", tabs.toYAML());
			steps.add(m);
		}
		if (trailingWhitespace!=null){
			Map<String,Object> m=new HashMap<>();
			m.put("trailing_whitespace", trailingWhitespace.toYAML());
			steps.add(m);
		}
		
		Map<String,Object> ret=new HashMap<>();
		ret.put("steps", steps);
		
		
		ret.put("columns", columns);
		
		if (languageExtensions.size()>0){
			ret.put("language_extensions", languageExtensions.toArray(new String[languageExtensions.size()]));
		}
		return ret;
	}
	
	public void clear(){
		imports=null;
		pragmas=null;
		tabs=null;
		trailingWhitespace=null;
		unicode=null;
		records=null;
		columns=DEFAULT_COLUMNS;
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
		} else if ("records".equalsIgnoreCase(name)){
			records=new SHRecords();
			records.fromYAML(params);
		} else if ("tabs".equalsIgnoreCase(name)){
			tabs=new SHTabs();
			tabs.fromYAML(params);
		} else if ("trailing_whitespace".equalsIgnoreCase(name)){
			trailingWhitespace=new SHTrailingWhitespace();
			trailingWhitespace.fromYAML(params);
		}
	}

	public int getColumns() {
		return columns;
	}

	public void setColumns(int columns) {
		this.columns = columns;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected SHConfiguration clone() {
		SHConfiguration clone=new SHConfiguration();
		clone.fromYAML(toYAML());
		return clone;
	}

	public SHRecords getRecords() {
		return records;
	}

	public void setRecords(SHRecords records) {
		this.records = records;
	}

	public Set<String> getLanguageExtensions() {
		return languageExtensions;
	}
}
