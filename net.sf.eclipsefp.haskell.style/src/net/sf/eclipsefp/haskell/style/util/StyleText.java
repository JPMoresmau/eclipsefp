/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.util;

import org.eclipse.osgi.util.NLS;

/**
 * Stylish Haskell text resources
 * @author JP Moresmau
 *
 */
public class StyleText extends NLS {
	
	
	public static String sh_unicode;
	public static String sh_unicode_pragmas;
	public static String sh_import;
	public static String sh_import_alignment;
	public static String sh_pragmas;
	public static String sh_pragmas_style;
	public static String sh_pragmas_remove_redundant;
	public static String sh_tabs;
	public static String sh_tabs_spaces;
	public static String sh_tabs_spaces_error;
	public static String sh_trailing_whitespace;

	public static String sh_title;
	public static String sh_project;
	public static String sh_default;
	
	public static String sh_save_error;
	
   private static final String BUNDLE_NAME = StyleText.class.getPackage().getName() + ".style"; //$NON-NLS-1$

  static {
    NLS.initializeMessages(BUNDLE_NAME, StyleText.class);
  }
}
