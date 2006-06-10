// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors;

import net.sf.eclipsefp.haskell.cabal.ui.internal.editors.text.CabalScanner;
import net.sf.eclipsefp.haskell.cabal.ui.internal.editors.text.CommentScanner;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

/** <p>the configuration for the cabal editor's source viewer.</p> 
  *
  * @author Leif Frenzel
  */
public class CabalConfiguration extends SourceViewerConfiguration {

  private ITokenScanner defaultScanner;
  private ITokenScanner commentScanner;
  
  
  // interface methods of SourceViewerConfiguration
  /////////////////////////////////////////////////
  
  public IPresentationReconciler getPresentationReconciler( final ISourceViewer sv ) {
    PresentationReconciler reconciler= new PresentationReconciler();
    
    DefaultDamagerRepairer dr = new DefaultDamagerRepairer( getDefaultScanner() );
    reconciler.setDamager( dr, IDocument.DEFAULT_CONTENT_TYPE );
    reconciler.setRepairer( dr, IDocument.DEFAULT_CONTENT_TYPE );

    DefaultDamagerRepairer cdr = new DefaultDamagerRepairer( getCommentScanner() );
    reconciler.setDamager( cdr, CabalDocProvider.COMMENT_CONTENT_TYPE );
    reconciler.setRepairer( cdr, CabalDocProvider.COMMENT_CONTENT_TYPE );
    
    return reconciler;
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
