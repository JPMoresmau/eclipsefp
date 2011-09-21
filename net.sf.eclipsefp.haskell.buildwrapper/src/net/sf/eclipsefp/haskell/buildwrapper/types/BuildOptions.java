package net.sf.eclipsefp.haskell.buildwrapper.types;

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
	private BWTarget target=BWTarget.Source;
	
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

	public BWTarget getTarget() {
		return target;
	}

	public BuildOptions setTarget(BWTarget target) {
		this.target = target;
		return this;
	}

	
	
}
