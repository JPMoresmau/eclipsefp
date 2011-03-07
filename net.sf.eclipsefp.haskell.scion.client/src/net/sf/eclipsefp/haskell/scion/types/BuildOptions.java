package net.sf.eclipsefp.haskell.scion.types;

import java.io.Serializable;

/**
 * 
 * @author JP Moresmau
 *
 */
public class BuildOptions implements Serializable {
	private boolean output;
	private boolean recompile;
	private boolean configure;

	public BuildOptions() {
		super();

	}

	public boolean isOutput() {
		return output;
	}

	public BuildOptions setOutput(boolean output) {
		this.output = output;
		return this;
	}

	public boolean isRecompile() {
		return recompile;
	}

	public BuildOptions setRecompile(boolean recompile) {
		this.recompile = recompile;
		return this;
	}

	public boolean isConfigure() {
		return configure;
	}

	public BuildOptions setConfigure(boolean configure) {
		this.configure = configure;
		return this;
	}

	
	
}
