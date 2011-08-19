package net.sf.eclipsefp.haskell.partitioned;

import java.io.InputStream;

public class HappyRunner extends PartitionedRunner {

	@Override
	public String getExecutableName() {
		return "happy";
	}

	@Override
	public InputStream selectStream(Process p) {
		return p.getErrorStream();
	}

}
