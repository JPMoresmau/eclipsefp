// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.ui.preferences;

import net.sf.eclipsefp.haskell.hugs.core.IHugsParameters;
import org.eclipse.osgi.util.NLS;


/** <p>Helper to provide texts for the various command line option names and
  * descriptions.</p>
  *
  * @author Leif Frenzel
  */
public class UITexts implements IHugsParameters {

//  private static final String SHORT_DESC = "ShortParamDescriptions";
 // private static ResourceBundle bundle = initBundle();

  static {
    NLS.initializeMessages( UITexts.class.getPackage()
        .getName()+".ShortParamDescriptions", UITexts.class );
  }

  public static String prefs_name;
  public static String prefs_notfound;

//
//  /** returns the actual option string. */
//  public static String getOption( final String key ) {
//    return key;
//  }
//
//  /** returns a short description text for the specified key. */
//  public static String getShortDescription( final String key ) {
//    String result = key;
//    if( bundle != null ) {
//      try {
//        String fromBundle = bundle.getString( key );
//        if( fromBundle != null ) {
//          result = fromBundle;
//        }
//      } catch( Exception ex ) {
//        log( "Could not find value for bundle key '" + key + "'.", ex );
//      }
//    }
//    return result;
//  }


  // helping methods
  //////////////////

//  private static ResourceBundle initBundle() {
//    ResourceBundle result = null;
//    try {
//      String name = UITexts.class.getPackage().getName() + "." + SHORT_DESC;
//      result = ResourceBundle.getBundle( name );
//    } catch( Exception ex ) {
//      log( "Could not init resource bundle.", ex );
//    }
//    return result;
//  }
//
//  private static void log( final String message, final Throwable thr ) {
//    HugsPlugin.log( message, thr );
//  }
}