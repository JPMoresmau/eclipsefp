/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

/**
 * Messages from Cabal that we can use to parse messages and take action
 * @author JP Moresmau
 *
 */
public class CabalMessages {
	public static final String RERUN_CONFIGURE=" re-run the 'configure'";
	public static final String CANNOT_SATISFY="cannot satisfy -package-id";
	public static final String VERSION="the version of cabal";
	
	public static final String DEPENDENCIES_MISSING="the following dependencies are missing";
	
	public static final String ANY="-any"; 
}
