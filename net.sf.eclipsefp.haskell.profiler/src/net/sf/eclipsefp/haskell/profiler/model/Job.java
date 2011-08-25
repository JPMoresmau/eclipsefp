/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.profiler.model;

import java.io.InputStream;
import java.io.Reader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.sf.eclipsefp.haskell.profiler.internal.parser.ParseException;
import net.sf.eclipsefp.haskell.profiler.internal.parser.ProfilingOutputParser;

/**
 * Represents an entire set of profiling output.
 * @author Alejandro Serrano
 *
 */
public class Job {
	String name;
	String date;
	String sampleUnit;
	String valueUnit;
	LinkedHashMap<Float, Sample> samples;
	
	public Job(String name, String date, String sampleUnit, String valueUnit) {
		this.name = name;
		this.date = date;
		this.sampleUnit = sampleUnit;
		this.valueUnit = valueUnit;
		this.samples = new LinkedHashMap<Float, Sample>();
	}

	public String getName() {
		return name;
	}

	public String getDate() {
		return date;
	}

	public String getSampleUnit() {
		return sampleUnit;
	}

	public String getValueUnit() {
		return valueUnit;
	}
	
	public Set<Map.Entry<Float, Sample>> getSamplesAndTimes() {
		return samples.entrySet();
	}
	
	public Collection<Sample> getSamples() {
		return samples.values();
	}
	
	public void addSample(Sample sample) {
		samples.put(sample.getTime(), sample);
	}
	
	public static Job parse(InputStream stream) throws ParseException {
		ProfilingOutputParser parser = new ProfilingOutputParser(stream);
		return parser.job();
	}
	
	public static Job parse(Reader stream) throws ParseException {
		ProfilingOutputParser parser = new ProfilingOutputParser(stream);
		return parser.job();
	}
	
	public BigInteger getTotal() {
		BigInteger result = BigInteger.ZERO;
		for (Sample s : getSamples())
			result = result.add(s.getTotal());
		return result;
	}
	
	public Map<String, BigInteger> getTotalByEntry() {
		LinkedHashMap<String, BigInteger> result = new LinkedHashMap<String, BigInteger>();
		for (Sample s : getSamples()) {
			for (Map.Entry<String, Long> e : s.getEntries()) {
				if (!result.containsKey(e.getKey())) {
					result.put(e.getKey(), BigInteger.ZERO);
				}
				BigInteger prev = result.get(e.getKey());
				result.put(e.getKey(), prev.add(BigInteger.valueOf(e.getValue())));
			}
		}
		return result;
	}
	
	public List<Map.Entry<String, BigInteger>> sortEntriesByTotal() {
		ArrayList<Map.Entry<String, BigInteger>> totals =
				new ArrayList<Map.Entry<String, BigInteger>>(
						getTotalByEntry().entrySet());
		Collections.sort(totals, new Comparator<Map.Entry<String, BigInteger>>() {
			public int compare(Entry<String, BigInteger> a, Entry<String, BigInteger> b) {
				return -a.getValue().compareTo(b.getValue());
			}
		});
		return totals;
	}
}
