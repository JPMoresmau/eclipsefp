// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;


/** <p>A singleton to manage scanner utilities (like tokens etc.)</p>
  *
  * @author Leif Frenzel
  */
public class ScannerManager implements IEditorPreferenceNames {

  /** the singleton instance of ScannerManager. */
  private static ScannerManager _instance;

  private HaskellCodeScanner codeScanner;
  private HaskellCommentScanner commentScanner;
  private HaskellStringScanner stringScanner;
  private HaskellCharacterScanner charScanner;
  private HaskellCommentScanner literateCommentScanner;

  private final Map<String, PropertyChangeHandler> propertyChangeHandlers;
  private IPropertyChangeListener propertyChangeListener;

  private TextAttribute commentAttribute;
  private TextAttribute literateCommentAttribute;

  private final Map<String, IToken> tokens = new Hashtable<String, IToken>();

  private IPreferenceStore prefStore;
  private ColorProvider colorProvider;

  private ScannerManager() {
    // prevent instantiation
    propertyChangeHandlers = new HashMap<String, PropertyChangeHandler>();
    initializePropertyListener();
  }

  /** <p>returns the singleton instance of ScannerManager.</p> */
  public static synchronized ScannerManager getInstance() {
    if( _instance == null ) {
      _instance = new ScannerManager();
    }
    return _instance;
  }

  public void dispose() {
    getPreferenceStore().removePropertyChangeListener( propertyChangeListener );
    if (colorProvider!=null){
      colorProvider.dispose();
    }
  }

  public ScannerManager(final IPreferenceStore prefStore){
    this.prefStore=prefStore;
    propertyChangeHandlers = new HashMap<String, PropertyChangeHandler>();
    initializePropertyListener();
  }


  public ColorProvider getColorProvider() {
    if (prefStore!=null){
      if (colorProvider==null){
        colorProvider=new ColorProvider( prefStore );
      }
      return colorProvider;
    }
    return ColorProvider.getInstance();
  }

  public HaskellCodeScanner getCodeScanner( final boolean latexLiterate ) {
    if( codeScanner == null ) {
      codeScanner = new HaskellCodeScanner( this,latexLiterate );
    }
    return codeScanner;
  }

  public HaskellCommentScanner getCommentScanner() {
    if( commentScanner == null ) {
      commentScanner = new HaskellCommentScanner( this,false );
    }
    return commentScanner;
  }

  public HaskellStringScanner getStringScanner() {
    if( stringScanner == null ) {
      stringScanner = new HaskellStringScanner(this);
    }
    return stringScanner;
  }

  public HaskellCharacterScanner getCharacterScanner() {
    if( charScanner == null ) {
      charScanner = new HaskellCharacterScanner(this);
    }
    return charScanner;
  }

  public HaskellCommentScanner getLiterateCommentScanner() {
    if( literateCommentScanner == null ) {
      literateCommentScanner = new HaskellCommentScanner( this,true );
    }
    return literateCommentScanner;
  }

  IToken createToken( final String colorKey, final String boldKey ) {
    IToken result = getTokenInternal( colorKey, boldKey );
    if( result == null ) {
      result = createTokenInternal( colorKey, boldKey );
    }
    return result;
  }

  TextAttribute getCommentAttribute() {
    if( commentAttribute == null ) {
      commentAttribute = createTextAttribute( EDITOR_COMMENT_COLOR,
                                              EDITOR_COMMENT_BOLD );
    }
    return commentAttribute;
  }

  TextAttribute getLiterateCommentAttribute() {
    if( literateCommentAttribute == null ) {
      String colorKey = EDITOR_LITERATE_COMMENT_COLOR;
      String boldKey = EDITOR_LITERATE_COMMENT_BOLD;
      literateCommentAttribute = createTextAttribute( colorKey, boldKey );
    }
    return literateCommentAttribute;
  }


  // helping methods
  //////////////////

  private TextAttribute createTextAttribute( final String colorKey,
                                             final String boldKey ) {
    Color color = getColorProvider().getColor( colorKey );
    TextAttribute result;
    if( isBold( boldKey ) ) {
      result = new TextAttribute( color, null, SWT.BOLD );
    } else {
      result = new TextAttribute( color );
    }
    return result;
  }

  private void initializePropertyListener() {
    propertyChangeListener = new IPropertyChangeListener() {
      public void propertyChange( final PropertyChangeEvent event ) {
        getColorProvider().changeColor( event.getProperty(),
                                                 event.getNewValue() );
        // now notify all tokens out there
        PropertyChangeHandler handler = getHandler( event.getProperty() );
        if( handler != null ) {
          handler.handleChange( event );
        }
      }
    };
    getPreferenceStore().addPropertyChangeListener( propertyChangeListener );
  }

  private void putHandler( final String key,
                           final PropertyChangeHandler handler ) {
    propertyChangeHandlers.put( key, handler );
  }

  private PropertyChangeHandler getHandler( final String propertyName ) {
    Object result = propertyChangeHandlers.get( propertyName );
    return ( PropertyChangeHandler )result;
  }

  private boolean isBold( final String boldKey ) {
    return getPreferenceStore().getBoolean( boldKey );
  }

  private IPreferenceStore getPreferenceStore() {
    if (prefStore!=null){
      return prefStore;
    }
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }

  private IToken getTokenInternal( final String colorKey,
                                   final String boldKey ) {
    return tokens.get( colorKey + boldKey );
  }

  private Token createTokenInternal( final String colorKey,
                                     final String boldKey ) {
    TextAttribute textAtt = createTextAttribute( colorKey,
                                                               boldKey );
    final Token result = new Token( textAtt );
    PropertyChangeHandler colorHandler = new PropertyChangeHandler() {
      @Override
      void handleChange( final PropertyChangeEvent event ) {
        handleColorChange( result, event.getProperty() );
      }
    };
    putHandler( colorKey, colorHandler );

    PropertyChangeHandler boldHandler = new PropertyChangeHandler() {
      @Override
      void handleChange( final PropertyChangeEvent event ) {
        handleBoldChange( result, event );
      }
    };
    putHandler( boldKey, boldHandler );
    tokens.put( colorKey + boldKey, result );
    return result;
  }

  private void handleColorChange( final Token token, final String key ) {
    Object data = token.getData();
    if( data instanceof TextAttribute ) {
      TextAttribute oldAttribute = ( TextAttribute )data;
      Color bgColor = oldAttribute.getBackground();
      int style = oldAttribute.getStyle();
      Color color = getColorProvider().getColor( key );
      TextAttribute newAttribute = new TextAttribute( color, bgColor, style );
      token.setData( newAttribute );
    }
  }

  private void handleBoldChange( final Token token,
                                 final PropertyChangeEvent event ) {
    Object o=event.getNewValue();

    boolean bold = (o instanceof Boolean)?
          ( ( Boolean )o ).booleanValue():
            IPreferenceStore.TRUE.equals(o);
    Object data = token.getData();
    if( data instanceof TextAttribute ) {
      TextAttribute oldAttr = ( TextAttribute )data;
      boolean wasBold = ( ( oldAttr.getStyle() & SWT.BOLD ) != 0 );
      if( wasBold != bold ) {
        int newStyle = bold ? oldAttr.getStyle() | SWT.BOLD
                            : oldAttr.getStyle() ^ SWT.BOLD;
        TextAttribute newAttribute = new TextAttribute( oldAttr.getForeground(),
                                                        oldAttr.getBackground(),
                                                        newStyle );
        token.setData( newAttribute );
      }
    }
  }


  // inner classes
  ////////////////

  private abstract class PropertyChangeHandler {
    abstract void handleChange( PropertyChangeEvent event );
  }
}