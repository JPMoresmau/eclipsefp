// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.preferences.overlay;


/** <p>Value objects for preference types in the overlay store.</p>
  * 
  * @author Leif Frenzel
  */
class OverlayType {

  public static final OverlayType BOOLEAN = new OverlayType();
  public static final OverlayType DOUBLE = new OverlayType();
  public static final OverlayType FLOAT = new OverlayType();
  public static final OverlayType INT = new OverlayType();
  public static final OverlayType LONG = new OverlayType();
  public static final OverlayType STRING = new OverlayType();

  private OverlayType() {
    // prevent instantiation
  }
}