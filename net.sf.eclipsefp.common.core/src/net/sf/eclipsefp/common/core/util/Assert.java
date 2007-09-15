// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.core.util;


/** <p>some simple assertion facilities.</p>
  * 
  * @author Leif Frenzel
  */
public class Assert {

  public static void isNotNull( final Object object ) {
    if( object == null ) {
      throw new AssertionFailedException();
    }
  }

  public static void isNotNull( final Object object, final String message ) {
    if( object == null ) {
      throw new AssertionFailedException( message );
    }
  }
  
  public static void isTrue( final boolean whatsClaimed ) {
    if( !whatsClaimed ) {
      throw new AssertionFailedException();
    }
  }

  public static void isTrue( final boolean whatsClaimed, 
                             final String message ) {
    if( !whatsClaimed ) {
      throw new AssertionFailedException( message );
    }
  }
  
  private static class AssertionFailedException extends RuntimeException {
    
    private static final long serialVersionUID = 1L;

	private AssertionFailedException() {
      super();
    }
    
    private AssertionFailedException( final String message ) {
      super( message );
    }
  }
}