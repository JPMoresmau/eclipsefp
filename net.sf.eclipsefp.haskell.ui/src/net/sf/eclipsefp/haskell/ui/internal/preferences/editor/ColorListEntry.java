// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

/** <p>struct that encapsulates some information about an item in a list from
  * which the user can select a color preference.</p> 
  * 
  * @author Leif Frenzel
  */
class ColorListEntry {
  
  private final String label;
  private final String colorKey;
  private String boldKey;

  ColorListEntry( final String label, 
                  final String colorKey ) {
    this.label = label;
    this.colorKey = colorKey;
  }
  
  ColorListEntry( final String label, 
                  final String colorKey, 
                  final String boldKey ) {
    this( label, colorKey );
    this.boldKey = boldKey;
  }
  
  
  // attribute setters and getters
  ////////////////////////////////
  
  String getBoldKey() {
    return boldKey;
  }

  String getColorKey() {
    return colorKey;
  }

  String getLabel() {
    return label;
  }
}