/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.profiler.model;

import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * Represents each of the samples that was taken
 * during a profiling session.
 * @author Alejandro Serrano
 *
 */
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
	
	public BigInteger getTotal() {
		BigInteger result = BigInteger.ZERO;
		for (long v : entries.values())
			result = result.add(BigInteger.valueOf(v));
		return result;
	}
}
