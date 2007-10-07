// Copyright (c) 2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.internal.refactoring;

import org.eclipse.osgi.util.NLS;

/** <p>provides internationalized String messages for the core.</p> 
  * 
  * @author Leif Frenzel
  */
public class CoreTexts {

  // message fields
  public static String mkPointFreeProcessor_elem;
  public static String mkPointFreeProcessor_name;

  public static String mkPointFreeDelegate_checking;
  public static String mkPointFreeDelegate_collectingChanges;
  public static String mkPointFreeDelegate_noSelection;
  public static String mkPointFreeDelegate_noSourceFile;
  public static String mkPointFreeDelegate_notApplicable;
  public static String mkPointFreeDelegate_roFile;

  
  // init stuff
  /////////////
  
  private static final String NAME =   CoreTexts.class.getPackage().getName()
                                     + ".coretexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( NAME, CoreTexts.class );
  }
}
