/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.visual.ui.text;

import org.eclipse.osgi.util.NLS;

/**
 * @author JP Moresmau
 *
 */
public class VisualTexts extends NLS {

	public static String desktop_bubble_new;
	
	private static final String BUNDLE_NAME = VisualTexts.class.getPackage()
		      .getName() + ".visual"; //$NON-NLS-1$


	  static {
	    NLS.initializeMessages( BUNDLE_NAME, VisualTexts.class );
	  }
}
