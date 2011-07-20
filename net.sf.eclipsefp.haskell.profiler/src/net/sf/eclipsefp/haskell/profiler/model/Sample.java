package net.sf.eclipsefp.haskell.profiler.model;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class Sample {
	float time;
	LinkedHashMap<String, Long> entries;
	
	public Sample(float time) {
		this.time = time;
		this.entries = new LinkedHashMap<String, Long>();
	}
	
	public float getTime() {
		return time;
	}
	
	public void addEntry(String name, long value) {
		entries.put(name, value);
	}
	
	public Set<Map.Entry<String, Long>> getEntries() {
		return entries.entrySet();
	}
}
