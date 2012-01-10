/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.browser.util;


import org.eclipse.osgi.util.NLS;

/**
 * messages from Browser
 * @author JP Moresmau
 *
 */
public class BrowserText extends NLS {
	
	public static String error_read;
	public static String error_loadlocaldb;
	public static String error_loadhackagedb;
	
	  private static final String BUNDLE_NAME = BrowserText.class.getPackage().getName() + ".browser"; //$NON-NLS-1$

	  static {
	    NLS.initializeMessages(BUNDLE_NAME, BrowserText.class);
	  }
}
