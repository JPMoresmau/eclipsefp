package net.sf.eclipsefp.haskell.profiler.internal.editors;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.sf.eclipsefp.haskell.profiler.model.Job;
import net.sf.eclipsefp.haskell.profiler.model.Sample;

public class ProfileNumbers {
	
	private LinkedHashMap<String, double[]> entries;
	private double[] rest;
	
	public ProfileNumbers(List<Map.Entry<String, BigInteger>> entriesApart, int noSamples) {
		this.entries = new LinkedHashMap<String, double[]>();
		for (Map.Entry<String, BigInteger> e : entriesApart) {
			double[] values = new double[noSamples];
			Arrays.fill(values, 0.0);
			entries.put(e.getKey(), values);
		}
		this.rest = new double[noSamples];
		Arrays.fill(this.rest, 0.0);
	}
	
	public void fillIn(Job job) {
		int sampleNo = 0;
		for (Sample s : job.getSamples()) {
			for (Map.Entry<String, Long> e : s.getEntries()) {
				if (entries.containsKey(e.getKey())) {
					entries.get(e.getKey())[sampleNo] = e.getValue();
				} else {
					rest[sampleNo] += e.getValue();
				}
			}
			sampleNo++;
		}
	}

	public LinkedHashMap<String, double[]> getEntries() {
		return entries;
	}

	public double[] getRest() {
		return rest;
	}
}
