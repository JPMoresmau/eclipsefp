/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import net.sf.eclipsefp.haskell.buildwrapper.types.GhcMessages;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.IDocument;
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

    //int line=marker.getAttribute(IMarker.LINE_NUMBER, 0);
  //  try {
      //IRegion r=document.getLineInformation( line-1 );
    // CHAR_START is from the document start!
      int c=marker.getAttribute(IMarker.CHAR_START, 0);
      // r.getOffset()+
      return new CompletionProposal( newS,c, oldS.length(), newS.length(),HaskellUIImages.getImage( IImageNames.CORRECTION ),getLabel(),null,null );
//    } catch( BadLocationException ex ) {
//      HaskellUIPlugin.log( ex );
//    }
//    return null;
  }

  public static List<String> getSuggestionsFromGHCMessage(final String msg){
    return getSuggestionsFromGHCMessage( msg ,msg.toLowerCase( Locale.ENGLISH ));
  }

  public static List<String> getSuggestionsFromGHCMessage(String msg,final String msgL){
    List<String> suggestions=new ArrayList<>();
    int start=msgL.indexOf( GhcMessages.NOT_IN_SCOPE_SUGGESTION_MULTIPLE );
    if (start==-1){
      start=msgL.indexOf( GhcMessages.NOT_IN_SCOPE_SUGGESTION );
      if (start>-1){
        start+=GhcMessages.NOT_IN_SCOPE_SUGGESTION.length();
      }
    } else {
      start+=GhcMessages.NOT_IN_SCOPE_SUGGESTION_MULTIPLE.length();
    }
    if (start==-1){
      return suggestions;
    }
    msg=msg.substring( start);
    int openParensIx=msg.indexOf( '(' );
    while (openParensIx>-1){
      String sug=msg.substring( 0,openParensIx );
      if (sug.startsWith( "," )){
        sug=sug.substring( 1 );
      }
      sug=sug.trim();
      if (sug.startsWith( "`") && sug.endsWith( "'" )){
        sug=sug.substring( 1,sug.length()-1 );
      }
      suggestions.add(sug);
      int closeParensIx=msg.indexOf( ')',openParensIx );
      if (closeParensIx>-1){
        msg=msg.substring( closeParensIx+1 );
        openParensIx=msg.indexOf( '(' );
      } else {
        openParensIx=-1;
      }
    }

    return suggestions;
  }
}
