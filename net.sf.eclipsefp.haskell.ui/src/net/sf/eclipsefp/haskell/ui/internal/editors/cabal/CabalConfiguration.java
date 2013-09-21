// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import java.util.Arrays;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.ca.CabalCompletionProcessor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text.CabalScanner;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text.CommentScanner;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellTextHover;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.AnnotationHover;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.resolve.QuickAssistProcessor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.DefaultLineTracker;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.TabsToSpacesConverter;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.URLHyperlinkDetector;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.quickassist.IQuickAssistAssistant;
import org.eclipse.jface.text.quickassist.QuickAssistAssistant;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.reconciler.MonoReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

/** <p>the configuration for the cabal editor's source viewer.</p>
  *
  * @author Leif Frenzel
  */
public class CabalConfiguration extends SourceViewerConfiguration {

  private final CabalFormEditor editor;

  private ITokenScanner defaultScanner;
  private ITokenScanner commentScanner;

  public CabalConfiguration( final CabalFormEditor editor ) {
    this.editor = editor;
  }


  // interface methods of SourceViewerConfiguration
  /////////////////////////////////////////////////

  @Override
  public ITextHover getTextHover( final ISourceViewer sourceViewer, final String contentType ) {
//    ITextHover result = null;
//    if( IDocument.DEFAULT_CONTENT_TYPE.equals( contentType ) ) {
//      result = new DefaultTextHover( sourceViewer ){
//        @Override
//        public String getHoverInfo( final ITextViewer textViewer, final IRegion hoverRegion ) {
//          return HaskellTextHover.computeProblemInfo( textViewer, hoverRegion, new DefaultMarkerAnnotationAccess() );
//        }
//
//
//      };
//    }
//    return result;
    return new HaskellTextHover(null,sourceViewer);
  }

  @Override
  public IPresentationReconciler getPresentationReconciler( final ISourceViewer sv ) {
    PresentationReconciler result= new PresentationReconciler();

    DefaultDamagerRepairer dr = new DefaultDamagerRepairer( getDefaultScanner() );
    result.setDamager( dr, IDocument.DEFAULT_CONTENT_TYPE );
    result.setRepairer( dr, IDocument.DEFAULT_CONTENT_TYPE );

    DefaultDamagerRepairer cdr = new DefaultDamagerRepairer( getCommentScanner() );
    result.setDamager( cdr, CabalDocProvider.COMMENT_CONTENT_TYPE );
    result.setRepairer( cdr, CabalDocProvider.COMMENT_CONTENT_TYPE );

    return result;
  }

  @Override
  public IAnnotationHover getAnnotationHover(final ISourceViewer sv) {
    return new AnnotationHover();
  }

  @Override
  public IReconciler getReconciler( final ISourceViewer sourceViewer ) {
    CabalReconcilingStrategy strategy = new CabalReconcilingStrategy( editor,sourceViewer );
    MonoReconciler result = new MonoReconciler( strategy, false );
    result.setDelay( 500 );
    return result;
  }

  @Override
  public IContentAssistant getContentAssistant( final ISourceViewer sv ) {
    ContentAssistant result = new ContentAssistant();
    IContentAssistProcessor processor = new CabalCompletionProcessor();
    result.setContentAssistProcessor( processor, IDocument.DEFAULT_CONTENT_TYPE );
    result.setContextInformationPopupOrientation( IContentAssistant.CONTEXT_INFO_ABOVE );
    result.setInformationControlCreator( getInformationControlCreator( sv ) );
    result.enableAutoInsert( true );
    return result;
  }

  @Override
  public IQuickAssistAssistant getQuickAssistAssistant(final ISourceViewer sourceViewer ) {
    QuickAssistAssistant qaa=new QuickAssistAssistant();
    qaa.setQuickAssistProcessor( new QuickAssistProcessor() );
    return qaa;
  }

  @Override
  public IHyperlinkDetector[] getHyperlinkDetectors( final ISourceViewer sv ) {
    IHyperlinkDetector[] result = null;
    if( sv != null) {
      result = new IHyperlinkDetector[] { new URLHyperlinkDetector(),
                                          new CabalHyperlinkDetector( editor ) };
    }
    return result;
  }


  @Override
  public String[] getIndentPrefixes( final ISourceViewer sourceViewer, final String contentType ) {
    int tabWidth = getTabWidth(sourceViewer);
    StringBuilder prefix = new StringBuilder();
    String[] ret=new String[tabWidth+2];
    for (int i = 0; i <= tabWidth; i++) {
      for (int j = 0; j + i < tabWidth; j++) {
        prefix.append(' ');
      }
      if (i != 0) {
        prefix.append(' ');
      }

      ret[i]=prefix.toString();
      prefix.setLength( 0 );
    }
    ret[tabWidth+1]=""; //$NON-NLS-1$
    return ret;
  }



  @Override
  protected String[] getIndentPrefixesForTab( final int tabWidth ) {
    String[] indentPrefixes= new String[tabWidth + 2];
    for (int i= 0; i <= tabWidth; i++) {
      char[] spaceChars= new char[i];
      Arrays.fill(spaceChars, ' ');
      String spaces= new String(spaceChars);
      if (i < tabWidth) {
        indentPrefixes[i]= spaces + ' ';
      } else {
        indentPrefixes[i]= new String(spaces);
      }
    }
    indentPrefixes[tabWidth + 1]= ""; //$NON-NLS-1$
    return indentPrefixes;
  }

  @Override
  public int getTabWidth( final ISourceViewer sourceViewer ) {
    return getPreferenceStore().getInt( IEditorPreferenceNames.EDITOR_CABAL_TAB_WIDTH );
  }

  @Override
  public IAutoEditStrategy[] getAutoEditStrategies( final ISourceViewer sourceViewer, final String contentType ) {
    TabsToSpacesConverter tabConverter = new TabsToSpacesConverter();
    tabConverter.setLineTracker( new DefaultLineTracker() );
    tabConverter.setNumberOfSpacesPerTab( getTabWidth( sourceViewer ) );
    return new IAutoEditStrategy[] {
        new CabalAutoIndentStrategy(),
        tabConverter
    };
  }

  @Override
  public String[] getConfiguredContentTypes( final ISourceViewer sourceViewer ) {
   return CabalDocProvider.TOKEN_TYPES;
  }

  @Override
  public String[] getDefaultPrefixes(final ISourceViewer sourceViewer, final String contentType) {
     return new String[] { "--" }; //$NON-NLS-1$
  }

  // helping methods
  //////////////////

  private ITokenScanner getDefaultScanner() {
    if( defaultScanner == null ) {
      defaultScanner = new CabalScanner();
    }
    return defaultScanner;
  }

  private ITokenScanner getCommentScanner() {
    if( commentScanner == null ) {
      commentScanner = new CommentScanner();
    }
    return commentScanner;
  }

  private IPreferenceStore getPreferenceStore() {
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }
}
