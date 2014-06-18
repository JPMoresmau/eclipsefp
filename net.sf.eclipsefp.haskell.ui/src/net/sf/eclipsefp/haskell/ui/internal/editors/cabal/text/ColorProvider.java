// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
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


  private static final RGB DEFAULT_COMMENT           = new RGB( 128, 128, 192 );
  private static final RGB DEFAULT_KEYWORD           = new RGB( 128, 0, 86 );
  private static final RGB DEFAULT_SECTION           = new RGB( 0,   128, 86 );
  private static final RGB DEFAULT_OTHER             = new RGB( 0, 0, 0 );

  private final Map<RGB, Color> colors;
  private final Map<String, RGB> rgbs;

  /** <p>constructs the singleton instance of ColorProvider. Private in order
   * to ensure the singleton pattern.</p> */
  public ColorProvider(final IPreferenceStore store) {
    colors = new HashMap<>( 10 );
    rgbs = new HashMap<>( 10 );
    initRgbs(store);
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

  private void initRgbs(final IPreferenceStore store) {
    putRgb( IEditorPreferenceNames.EDITOR_COMMENT_COLOR, DEFAULT_COMMENT ,store);
    putRgb( IEditorPreferenceNames.EDITOR_KEYWORD_COLOR, DEFAULT_KEYWORD ,store);
    putRgb( IEditorPreferenceNames.EDITOR_CON_COLOR, DEFAULT_SECTION ,store);
    putRgb( IEditorPreferenceNames.EDITOR_SYMBOL_COLOR, DEFAULT_OTHER ,store);
  }

  private void putRgb( final String key, final RGB defaultRgb ,final IPreferenceStore store) {
    RGB rgb = PreferenceConverter.getColor( store, key );
    if( rgb == null ) {
      rgb = defaultRgb;
    }
    rgbs.put( key, rgb );
  }
}