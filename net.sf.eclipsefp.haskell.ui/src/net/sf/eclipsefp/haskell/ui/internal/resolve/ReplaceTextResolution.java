/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;


/**
 * Replace some text at the marker position by another
 * @author JP Moresmau
 *
 */
public class ReplaceTextResolution extends MarkerCompletion {
  /**
   * the old string
   */
  private final String oldS;
  /**
   * the new string
   */
  private final String newS;



  public ReplaceTextResolution( final String oldS, final String newS ) {
    super();
    this.oldS = oldS;
    this.newS = newS;
  }

  @Override
  public String getLabel() {
    return NLS.bind( UITexts.resolve_text_replace, oldS, newS );
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {

    int line=marker.getAttribute(IMarker.LINE_NUMBER, 0);
    try {
      IRegion r=document.getLineInformation( line-1 );
      int c=marker.getAttribute(IMarker.CHAR_START, 0);
      return new CompletionProposal( newS, r.getOffset()+c, oldS.length(), newS.length(),HaskellUIImages.getImage( IImageNames.CORRECTION ),getLabel(),null,null );
    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( ex );
    }
    return null;
  }
}
