package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.StringWriter;

/**
 * Runner for Alex lexer tool.
 *
 * @author Alejandro Serrano
 */
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
