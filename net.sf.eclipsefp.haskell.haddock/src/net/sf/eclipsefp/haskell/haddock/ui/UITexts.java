// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui;

import java.util.ResourceBundle;

import net.sf.eclipsefp.haskell.haddock.HaddockPlugin;
import net.sf.eclipsefp.haskell.haddock.core.IHaddockParameters;


/** <p>Helper to provide texts for the various compiler option names and
  * descriptions.</p>
  *
  * @author Leif Frenzel
  */
public class UITexts implements IHaddockParameters {

  private static final String SHORT_DESC = "ShortParamDescriptions";
  private static ResourceBundle bundle = initBundle();
  
  /** returns the actual option string for Haddock. */
  public static String getOption( final String key ) {
    return key;
  }

  /** returns a short description text for the specified key. */
  public static String getShortDescription( final String key ) {
    String result = key;
    if( bundle != null ) {
      try {
        String fromBundle = bundle.getString( key );
        if( fromBundle != null ) {
          result = fromBundle;
        }
      } catch( Exception ex ) {
        log( "Could not find value for bundle key '" + key + "'.", ex );
      }
    }
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private static ResourceBundle initBundle() {
    ResourceBundle result = null;
    try {
      String name = UITexts.class.getPackage().getName() + "." + SHORT_DESC;
      result = ResourceBundle.getBundle( name );
    } catch( Exception ex ) {
      log( "Could not init resource bundle.", ex );
    }
    return result;
  }
  
  private static void log( final String message, final Throwable thr ) {
    HaddockPlugin.log( message, thr );
  }
}