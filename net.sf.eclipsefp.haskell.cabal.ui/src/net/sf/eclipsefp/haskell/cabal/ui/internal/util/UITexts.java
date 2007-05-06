// Copyright (c) 2006-2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.util;

import org.eclipse.osgi.util.NLS;

/** <p>provides access to the internationalized UI texts.</p>
  * 
  * @author Leif Frenzel
  */
public final class UITexts extends NLS {

  public static String cabalHyperLinkDetector_errorTitle;
  
  private static final String BUNDLE_NAME 
    = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}