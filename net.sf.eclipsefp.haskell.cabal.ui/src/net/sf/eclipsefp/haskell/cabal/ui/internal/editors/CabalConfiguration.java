// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors;

import net.sf.eclipsefp.haskell.cabal.ui.internal.editors.ca.CabalCompletionProcessor;
import net.sf.eclipsefp.haskell.cabal.ui.internal.editors.text.CabalScanner;
import net.sf.eclipsefp.haskell.cabal.ui.internal.editors.text.CommentScanner;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.reconciler.MonoReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

/** <p>the configuration for the cabal editor's source viewer.</p> 
  *
  * @author Leif Frenzel
  */
class CabalConfiguration extends SourceViewerConfiguration {

  private final CabalEditor editor;

  private ITokenScanner defaultScanner;
  private ITokenScanner commentScanner;
  
  CabalConfiguration( final CabalEditor editor ) {
    this.editor = editor;
  }  
  
  
  // interface methods of SourceViewerConfiguration
  /////////////////////////////////////////////////
  
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

  public IReconciler getReconciler( final ISourceViewer sourceViewer ) {
    CabalReconcilingStrategy strategy = new CabalReconcilingStrategy( editor );
    MonoReconciler result = new MonoReconciler( strategy, false );
    result.setDelay( 500 );
    return result;
  }

  public IContentAssistant getContentAssistant( final ISourceViewer sv ) {
    ContentAssistant result = new ContentAssistant();
    IContentAssistProcessor processor = new CabalCompletionProcessor();
    result.setContentAssistProcessor( processor, 
                                      IDocument.DEFAULT_CONTENT_TYPE );
    result.setContextInformationPopupOrientation( 
                                         IContentAssistant.CONTEXT_INFO_ABOVE );
    result.setInformationControlCreator( getInformationControlCreator( sv ) );
    result.enableAutoInsert( true );
    return result;
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
}
