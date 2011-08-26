package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.StringWriter;

/**
 * Runner for Happy parser tool.
 *
 * @author Alejandro Serrano
 */
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
