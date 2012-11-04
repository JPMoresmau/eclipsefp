// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import java.io.InputStream;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellDocumentProvider;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellSourceViewerConfiguration;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/** <p>A source viewer that uses the Haskell editor's presentation helpers
  * in order to display a preview of the current presentation preference
  * settings.</p>
  *
  * @author Leif Frenzel
  */
public class SyntaxPreviewer extends SourceViewer implements IEditorPreferenceNames {

  private IPreferenceStore store;
  private final SourceViewerConfiguration config;
  private IPropertyChangeListener propertyChangeListener;
  private Color bgColor;

  /**
   * show the preview content
   * @param parent the UI parent
   * @param store the preferences store to get color preferences from
   */
  SyntaxPreviewer( final Composite parent, final IPreferenceStore store ) {
    this(parent,store,new HaskellSourceViewerConfiguration( null ), loadTextFromResource( "preview.hs" ));
  }

  /**
   * show arbitrary content
   * @param parent the UI parent
   * @param store the preferences store to get color preferences from
   * @param config the source viewer configuration
   * @param content the document contents
   */
  public SyntaxPreviewer( final Composite parent, final IPreferenceStore store,final SourceViewerConfiguration config,final String content ) {
    super( parent, null, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER );
    this.store = store;

    this.config=config;
    if (config instanceof HaskellSourceViewerConfiguration){
     ((HaskellSourceViewerConfiguration)config).setPreferenceStore( store );
    }
    configure( config );
    //done after configure so that the text invalidation is done after the scanner manager being notified
    initializePropertyListener();
    setEditable( false );
    getTextWidget().setFont( JFaceResources.getTextFont() );

    IDocument document = new Document( content);
    setupDocument( document, IDocumentExtension3.DEFAULT_PARTITIONING );
    setDocument( document );
  }


  @Override
  protected void handleDispose() {
    if( store != null ) {
      if( propertyChangeListener != null ) {
        store.removePropertyChangeListener( this.propertyChangeListener );
        propertyChangeListener = null;
      }
      if (config instanceof HaskellSourceViewerConfiguration){
        ((HaskellSourceViewerConfiguration)config).getScannerManager().dispose();
      }
      store = null;
    }
    if( ( this.bgColor != null )
        && !this.bgColor.isDisposed() ) {
      this.bgColor.dispose();
    }
    super.handleDispose();
  }


  // helping methods
  //////////////////

  private boolean affectsPresentation( final PropertyChangeEvent event ) {
    String p = event.getProperty();
    if(    EDITOR_BACKGROUND_COLOR.equals( p )
        || EDITOR_BACKGROUND_DEFAULT_COLOR.equals( p ) ) {
      return true;
    }
    /*for (ColorListEntry cle:SyntaxTab.colorListModel){
      if (p.equals(cle.getBoldKey()) || p.equals( cle.getColorKey() )){
        return true;
      }
    }*/
    return false;
  }

  /** Creates a color from the information stored in the given preference
   * store.   */
  private Color createColor( final String key, final Display display ) {
    Color color = null;
    if( this.store.contains( key ) ) {
      RGB rgb = null;
      if( this.store.isDefault( key ) ) {
        rgb = PreferenceConverter.getDefaultColor( this.store, key );
      } else {
        rgb = PreferenceConverter.getColor( this.store, key );
      }
      if( rgb != null ) {
        color = new Color( display, rgb );
      }
    }
    return color;
  }

  private static String loadTextFromResource( final String name ) {
    String result = ""; //$NON-NLS-1$
    try {
      InputStream stream = SyntaxPreviewer.class.getResourceAsStream( name );
      result = ResourceUtil.readStream( stream );
    } catch( Exception ex ) {
      HaskellUIPlugin.log( "Could not read preview file.", ex ); //$NON-NLS-1$
    }
    return result;
  }

  private void updateColors() {
    if( this.store != null ) {
      StyledText styledText = getTextWidget();
      Color color = null;
      if( !this.store.getBoolean( EDITOR_BACKGROUND_DEFAULT_COLOR ) ) {
        color = createColor( EDITOR_BACKGROUND_COLOR, styledText.getDisplay() );
      }
      styledText.setBackground( color );
      if( this.bgColor != null ) {
        this.bgColor.dispose();
      }
      this.bgColor = color;
    }
  }

  private void initializePropertyListener() {
    propertyChangeListener = new IPropertyChangeListener() {
      @Override
      public void propertyChange( final PropertyChangeEvent event ) {
        if( affectsPresentation( event ) ) {
          updateColors();
        }
        invalidateTextPresentation();
       }
    };
    store.addPropertyChangeListener( this.propertyChangeListener );
  }

  private void setupDocument( final IDocument document,
                              final String partitioning ) {
    IDocumentPartitioner partitioner = getPartitioner();
    if( document instanceof IDocumentExtension3 ) {
      IDocumentExtension3 extension = ( IDocumentExtension3 )document;
      extension.setDocumentPartitioner( partitioning, partitioner );
    } else {
      document.setDocumentPartitioner( partitioner );
    }
    partitioner.connect( document );
  }

  private IDocumentPartitioner getPartitioner() {
    return HaskellDocumentProvider.createDocumentPartitioner();
  }
}