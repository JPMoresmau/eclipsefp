// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core;

/** <p>The common exception type for exceptions that occure in the Haskell 
  * Core classes.</p> 
  * 
  * @author Leif Frenzel
  */
public class HaskellCoreException extends Exception {

  private static final long serialVersionUID = -721302438783076582L;

  /** <p>constructs a new HaskellCoreException with the specified 
    * message.</p> */
  public HaskellCoreException( final String message ) {
    super( message );
  }
}