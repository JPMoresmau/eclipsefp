// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors.text;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;


/** <p>Provides colors for syntax coloring in the editor.</p>
  * 
  * <p>This is implemented as singleton to make it accessible from 
  * everywhere and also to reduce resource management to one single place.</p>
  * 
  * @author Leif Frenzel
  */
public class ColorProvider {

  static final String COMMENT = "COMMENT"; //$NON-NLS-1$
  static final String KEYWORD = "KEYWORD"; //$NON-NLS-1$
  static final String OTHER   = "OTHER"; //$NON-NLS-1$

  private static final RGB DEFAULT_COMMENT           = new RGB( 128, 128, 192 );
  private static final RGB DEFAULT_KEYWORD           = new RGB( 128, 0, 86 );
  private static final RGB DEFAULT_OTHER             = new RGB( 0, 0, 0 );

  /** the singleton instance of ColorProvider. */
  private static ColorProvider _instance;

  private Map<RGB, Color> colors;
  private Map<String, RGB> rgbs;

  /** <p>constructs the singleton instance of ColorProvider. Private in order
   * to ensure the singleton pattern.</p> */
  private ColorProvider() {
    colors = new HashMap<RGB, Color>( 10 );
    rgbs = new HashMap<String, RGB>( 10 );
    initRgbs();
  }

  public static synchronized ColorProvider getInstance() {
    if( _instance == null ) {
      _instance = new ColorProvider();
    }
    return _instance;
  }

  /** <p>releases all of the color resources held by this ColorProvider.</p> */ 
  public void dispose() {
    Iterator<Color> it = colors.values().iterator();
    while( it.hasNext() ) {
      it.next().dispose();
    }
  }

  public Color getColor( final String key ) {
    Color result = null;
    RGB rgb = rgbs.get( key );
    if( rgbs.containsKey( key ) ) {
      result = getColor( rgb );
    }
    return result;
  }
  

  // helping methods
  //////////////////

  private Color getColor( final RGB rgb ) {
    Color color = colors.get( rgb );
    if( color == null ) {
      color = new Color( Display.getCurrent(), rgb );
      colors.put( rgb, color );
    }
    return color;
  }
  
  private void initRgbs() {
    rgbs.put( COMMENT, DEFAULT_COMMENT );
    rgbs.put( KEYWORD, DEFAULT_KEYWORD );
    rgbs.put( OTHER, DEFAULT_OTHER );
  }
}