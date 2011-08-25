// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.profiler.internal.util;

import org.eclipse.osgi.util.NLS;

/** <p>provides access to the internationalized UI texts.</p>
  *
  * @author Leif Frenzel
  * @author Alejandro Serrano
  */
public final class UITexts extends NLS {

  // message fields
  public static String graph_restOfTrace;
  public static String graph_title;
  public static String graph_options;
  public static String graph_ungroupElements;

  private static final String BUNDLE_NAME = UITexts.class.getPackage().getName() + ".uitexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, UITexts.class );
  }
}