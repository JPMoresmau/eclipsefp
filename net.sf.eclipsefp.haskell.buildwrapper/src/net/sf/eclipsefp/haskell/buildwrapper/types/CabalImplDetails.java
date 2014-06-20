/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.ArrayList;
import java.util.List;

/**
 * Structure holding implementation of cabal with options especially for sandboxing
 * @author JP Moresmau
 * 
 */
public class CabalImplDetails {
	public enum SandboxType {
		NONE, CABAL_DEV, CABAL;
	}

	private String executable;
	private final List<String> options = new ArrayList<>();
	private final List<String> installOptions = new ArrayList<>();
	private final List<String> initOptions = new ArrayList<>();
	
	private SandboxType type = SandboxType.NONE;

	private String sandboxPath;
	


	/**
	 * do we have a unique sandbox?
	 */
	private boolean uniqueSandbox = false;

	public CabalImplDetails() {
		super();
	}

	public String getExecutable() {
		return executable;
	}

	public List<String> getOptions() {
		return options;
	}

	public void setExecutable(final String executable) {
		this.executable = executable;
	}

	public boolean isSandboxed() {
		return !getType().equals(SandboxType.NONE);
	}

	public SandboxType getType() {
		return type;
	}

	public void setType(SandboxType type) {
		this.type = type;
	}

	public List<String> getInstallOptions() {
		return installOptions;
	}

	/**
	 * @return the initOptions
	 */
	public List<String> getInitOptions() {
		return initOptions;
	}
	
	public boolean isUniqueSandbox() {
		return uniqueSandbox;
	}

	public void setUniqueSandbox(boolean uniqueSandbox) {
		this.uniqueSandbox = uniqueSandbox;
	}

	public String getSandboxPath() {
		return sandboxPath;
	}

	public void setSandboxPath(String sandboxPath) {
		this.sandboxPath = sandboxPath;
	}
}
