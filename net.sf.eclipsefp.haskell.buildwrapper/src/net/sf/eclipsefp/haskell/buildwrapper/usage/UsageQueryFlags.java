/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.usage;

/**
 * Bit fields for query flags
 * @author JP Moresmau
 *
 */
public class UsageQueryFlags {
	public static final int TYPE_TYPE=1;
	public static final int TYPE_CONSTRUCTOR=2;
	public static final int TYPE_VAR=4;
	public static final int TYPE_MODULE=8;
	
	public static final int TYPE_ALL=TYPE_TYPE | TYPE_CONSTRUCTOR | TYPE_VAR | TYPE_MODULE;
	
	public static final int SCOPE_DEFINITIONS=1;
	public static final int SCOPE_REFERENCES=2;
	
	public static final int SCOPE_ALL=SCOPE_DEFINITIONS | SCOPE_REFERENCES;
}
