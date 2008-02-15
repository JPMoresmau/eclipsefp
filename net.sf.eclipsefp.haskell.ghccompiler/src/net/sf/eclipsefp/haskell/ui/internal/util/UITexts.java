// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.util;

import org.eclipse.osgi.util.NLS;

/** <p>provides access to the internationalized UI texts.</p>
  *
  * @author Leif Frenzel
  */
public final class UITexts extends NLS {

  // message fields
  public static String implementationsBlock_btnAdd;
  public static String implementationsBlock_btnEdit;
  public static String implementationsBlock_btnRemove;
  public static String implementationsBlock_colName;
  public static String implementationsBlock_colType;
  public static String implementationsBlock_colVersion;
  public static String implementationsBlock_installed;

  public static String installedImplementationsPP_msg;
  public static String installedImplementationsPP_nothingSelected;

  private static final String BUNDLE_NAME
    = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}