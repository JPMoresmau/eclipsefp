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

  /** The internal singleton reference to ScannerManager */
  private static class SingletonHolder {
    private static final ScannerManager theInstance = new ScannerManager();
  }

  @Deprecated
  private HaskellCodeScanner codeScanner;
  @Deprecated
  private HaskellCommentScanner commentScanner;
  @Deprecated
  private HaskellStringScanner stringScanner;
  @Deprecated
  private HaskellCharacterScanner charScanner;
  @Deprecated
  private HaskellCommentScanner literateCommentScanner;

  private final Map<String, PropertyChangeHandler> propertyChangeHandlers;
  private IPropertyChangeListener propertyChangeListener;

  private TextAttribute commentAttribute;
  private TextAttribute literateCommentAttribute;

  private final Map<String, IToken> tokens = new Hashtable<>();

  private IPreferenceStore prefStore;
  private ColorProvider colorProvider;

  private ScannerManager() {
    // prevent instantiation
    propertyChangeHandlers = new HashMap<>();
    initializePropertyListener();
  }

  /** <p>returns the singleton instance of ScannerManager.</p> */
  public static final ScannerManager getInstance() {
    return SingletonHolder.theInstance;
  }

  public void dispose() {
    getPreferenceStore().removePropertyChangeListener( propertyChangeListener );
    if (colorProvider!=null){
      colorProvider.dispose();
    }
    tokens.clear();
  }

  public ScannerManager(final IPreferenceStore prefStore){
    this.prefStore=prefStore;
    propertyChangeHandlers = new HashMap<>();
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

  @Deprecated
  public HaskellCodeScanner getCodeScanner( final boolean latexLiterate ) {
    if( codeScanner == null ) {
      codeScanner = new HaskellCodeScanner( this,latexLiterate );
    }
    return codeScanner;
  }

  @Deprecated
  public HaskellCommentScanner getCommentScanner() {
    if( commentScanner == null ) {
      commentScanner = new HaskellCommentScanner( this,false );
    }
    return commentScanner;
  }

  @Deprecated
  public HaskellStringScanner getStringScanner() {
    if( stringScanner == null ) {
      stringScanner = new HaskellStringScanner(this);
    }
    return stringScanner;
  }

  @Deprecated
  public HaskellCharacterScanner getCharacterScanner() {
    if( charScanner == null ) {
      charScanner = new HaskellCharacterScanner(this);
    }
    return charScanner;
  }

  @Deprecated
  public HaskellCommentScanner getLiterateCommentScanner() {
    if( literateCommentScanner == null ) {
      literateCommentScanner = new HaskellCommentScanner( this,true );
    }
    return literateCommentScanner;
  }

  public IToken createToken( final String colorKey, final String boldKey ) {
    IToken result = getTokenInternal( colorKey, boldKey );
    if( result == null ) {
      result = createTokenInternal( colorKey, boldKey );
    }
    return result;
  }

  public TextAttribute getCommentAttribute() {
    if( commentAttribute == null ) {
      commentAttribute = createTextAttribute( EDITOR_COMMENT_COLOR,
                                              EDITOR_COMMENT_BOLD );
    }
    return commentAttribute;
  }

  public TextAttribute getLiterateCommentAttribute() {
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
      @Override
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

  public IPreferenceStore getPreferenceStore() {
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
    TextAttribute textAtt = createTextAttribute( colorKey, boldKey );
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