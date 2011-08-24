package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.StringWriter;

public class AlexRunner extends PartitionedRunner {

	@Override
	public String getExecutableName() {
		return "alex";
	}

  @Override
  public StringWriter selectStream( final StringWriter out, final StringWriter err ) {
    return err;
  }

}
