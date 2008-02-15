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
  public static String mkPointFree_refuseDlg_title;
  public static String mkPointFree_refuseDlg_message;

  public static String mkPointFreeProcessor_elem;
  public static String mkPointFreeProcessor_name;

  public static String mkPointFreeDelegate_checking;
  public static String mkPointFreeDelegate_collectingChanges;
  public static String mkPointFreeDelegate_noSelection;
  public static String mkPointFreeDelegate_noSourceFile;
  public static String mkPointFreeDelegate_notApplicable;
  public static String mkPointFreeDelegate_roFile;



  private static final String BUNDLE_NAME
    = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}