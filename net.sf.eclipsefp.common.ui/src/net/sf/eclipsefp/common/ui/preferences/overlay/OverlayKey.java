// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.preferences.overlay;


/** <p>encapsulates a typed key for the overlay store.</p>
  * 
  * @author Leif Frenzel
  */
class OverlayKey {

  private final OverlayType type;
  private final String key;

  OverlayKey( final OverlayType type, final String key ) {
    this.type = type;
    this.key = key;
  }
  
  
  // attribute getters and setters
  ////////////////////////////////
  
  String getKey() {
    return key;
  }
  
  OverlayType getType() {
    return type;
  }
}