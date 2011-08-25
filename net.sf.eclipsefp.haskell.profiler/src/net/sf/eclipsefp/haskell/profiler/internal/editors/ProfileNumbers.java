/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.profiler.internal.editors;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.sf.eclipsefp.haskell.profiler.model.Job;
import net.sf.eclipsefp.haskell.profiler.model.Sample;

/**
 * Generates the data to be shown in the viewer.
 * It gets the n most important measures (specified by the
 * noSamples argument to the constructor) and groups the
 * rest into another group.
 * @author Alejandro Serrano
 *
 */
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
	
	/**
	 * Group the elements specified according to a specific job.
	 * @param job The job with the samples.
	 */
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

	/**
	 * Gets information about ungrouped elements.
	 * @return Each key-value pair shows the name of the
	 *         element and its samples.
	 */
	public LinkedHashMap<String, double[]> getEntries() {
		return entries;
	}

	/**
	 * Get information about the grouped elements.
	 * @return The added samples of all grouped elements.
	 */
	public double[] getRest() {
		return rest;
	}
}
