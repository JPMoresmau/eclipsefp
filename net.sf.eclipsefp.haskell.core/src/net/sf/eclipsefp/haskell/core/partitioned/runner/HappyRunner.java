package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.StringWriter;

public class HappyRunner extends PartitionedRunner {

	@Override
	public String getExecutableName() {
		return "happy";
	}

  @Override
  public StringWriter selectStream( final StringWriter out, final StringWriter err ) {
    return err;
  }

}
