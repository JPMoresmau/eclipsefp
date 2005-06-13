// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.editor.codeassist;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;

import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;
import de.leiffrenzel.fp.haskell.ui.editor.syntax.HaskellSyntax;

/** <p>computes the code assist completion proposals and context 
  * information.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellCAProcessor implements IContentAssistProcessor {

  // interface methods of IContentAssistProcessor
  ///////////////////////////////////////////////
  
  public ICompletionProposal[] computeCompletionProposals(  
                                  final ITextViewer viewer, final int offset ) {

    IDocument doc = viewer.getDocument();
    String mask = "";
    try {
      mask = getQualifier( doc, offset );
    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( "Problem while determining start of proposal.", ex );
    }
    return computeProposals( mask, offset );
  }

  public IContextInformation[] computeContextInformation(
                          final ITextViewer viewer, final int documentOffset ) {
    // TODO Auto-generated method stub
    return null;
  }

  public char[] getCompletionProposalAutoActivationCharacters() {
    // TODO get from pref and update on pref change
//    return new char[] { '.' };
    return null;
  }

  public char[] getContextInformationAutoActivationCharacters() {
    // TODO Auto-generated method stub
    return null;
  }

  public String getErrorMessage() {
    // return null to indicate we had no problems
    return null;
  }

  public IContextInformationValidator getContextInformationValidator() {
    // TODO Auto-generated method stub
    return null;
  }
  
  
  // helping methods
  //////////////////
  
  private String getQualifier( final IDocument doc, 
                               final int offset ) throws BadLocationException {
    int index = offset;
    StringBuffer sb = new StringBuffer();
    String result = "";
    
    boolean finished = false;
    while( !finished && index > 0 ) {
      char ch = doc.getChar( --index );
      if( Character.isLetterOrDigit( ch ) ) {
        sb.append( ch );
      } else if( ch == '\"' || ch == '\'' ) {
        // striong or char literals are not taken into account
        finished = true;
      } else {
        // no ore identifier part, so we use what we have collected
        result = sb.reverse().toString();
        finished = true;
      }
    }
    if( index == 0 ) {
      // the special case where we have collected sth. but have reached the
      // end of the document meanwhile
      result = sb.reverse().toString();
    }
    return result;
  }
  
  private ICompletionProposal[] computeProposals( final String mask,
                                                  final int offset ) {
    ArrayList alResult = new ArrayList();
    computeKeywordCompletions( mask, offset, alResult );
    computeClassCompletions( mask, offset, alResult );
    return toArray( alResult );
  }

  private void computeKeywordCompletions( final String mask, 
                                          final int offset, 
                                          final ArrayList al ) {
    String[] keywords = HaskellSyntax.getKeywords();
    for( int i = 0; i < keywords.length; i++ ) {
      String kw = keywords[ i ];
      if( kw.startsWith( mask ) ) {
        int len = mask.length();
        al.add( new CompletionProposal( kw, offset - len, len, offset + len ) );
      }
    }
  }

  private void computeClassCompletions( final String mask, 
                                        final int offset, 
                                        final ArrayList al ) {
    String[] classes = HaskellSyntax.getClasses();
    for( int i = 0; i < classes.length; i++ ) {
      String kw = classes[ i ];
      if( kw.startsWith( mask ) ) {
        int len = mask.length();
        // TODO img
        al.add( new CompletionProposal( kw, 
                                        offset - len, 
                                        len, 
                                        offset + len ) );
      }
    }
  }

  private ICompletionProposal[] toArray( final List list ) {
    ICompletionProposal[] result = new ICompletionProposal[ list.size() ];
    list.toArray( result );
    return result;
  }
}