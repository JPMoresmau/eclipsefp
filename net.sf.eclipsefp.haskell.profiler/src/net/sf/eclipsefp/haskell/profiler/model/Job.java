package net.sf.eclipsefp.haskell.profiler.model;

import java.io.InputStream;
import java.io.Reader;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.profiler.internal.parser.ParseException;
import net.sf.eclipsefp.haskell.profiler.internal.parser.ProfilingOutputParser;

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
}
