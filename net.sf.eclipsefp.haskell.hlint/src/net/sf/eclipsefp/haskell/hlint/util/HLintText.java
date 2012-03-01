/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.hlint.util;

import org.eclipse.osgi.util.NLS;

/**
 * @author JP Moresmau
 *
 */
public class HLintText extends NLS {
	  public static String 		 error_run;
	  public static String 		suggestion_found;
      public static String 		suggestion_post;
	  
	  private static final String BUNDLE_NAME = HLintText.class.getPackage().getName() + ".hlint"; //$NON-NLS-1$

	  static {
	    NLS.initializeMessages(BUNDLE_NAME, HLintText.class);
	  }
}
